package skakapo.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.staticcontent.*
import skakapo.agent.*
import skakapo.api.*
import sbirds.auth.{OidcClient, OidcConfig}
import skakapo.auth.SecretBox
import skakapo.db.*
import sbirds.observe.{HttpClientTelemetry, HttpTelemetry, Log, Metrics, RumRoutes, Telemetry}
import sbirds.push.{PushService, PushStore}
import skakapo.core.ErrorCode
import sbirds.http.Errors

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

final case class AppConfig(port: Int,
                           db: DbConfig,
                           oidc: Option[OidcConfig],
                           api: ApiConfig,
                           staticDir: Option[String],
                           appSecret: Option[String],
                           node: String,
                           slots: Int,
                           workDir: String,
                           bindHost: com.comcast.ip4s.Host)

object Main extends IOApp.Simple {

  // ログにトレースの ID・操作したユーザーを載せるため、IOLocal の文脈を Java のライブラリから見えるようにする
  // （IORuntime を作る前に設定する必要がある. オブジェクトの初期化は main より先に動く）
  Telemetry.prepare()

  private val version =
    Option(getClass.getPackage.getImplementationVersion).orElse(sys.env.get("APP_VERSION")).getOrElse("dev")

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for {
      cfg <- Config.load
      n <- migrateWithRetry(cfg)
      _ <- Log.println(s"[skakapo] database migrated ($n migration(s) applied)")
      _ <- (for {
        _ <- Telemetry.resource("skakapo", version, runtime)
        xa <- Database.transactor(cfg.db)
        _ <- gauges(xa)
        // 外への呼び出し（IdP・通知先・プッシュのサービス）もトレースに出す（traceparent を付けて送る）
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build.map(HttpClientTelemetry(_))
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        users = new UserStore(xa)
        sess = new SessionAuth(users, cfg.api)
        box = cfg.appSecret.map(SecretBox(_))
        config = new ConfigStore(xa)
        connStore = new ConnectionStore(xa)
        store = new SessionStore(xa)
        nodes = new NodeStore(xa)
        mcp = new Mcp(http)
        conns = new Connections(config, connStore, box, http, mcp, cfg.api.publicUrl)
        workBase = java.nio.file.Path.of(cfg.workDir).toAbsolutePath.normalize()
        _ <- Resource.eval(config.seedDefaults)
        _ <- Resource.eval(IO.whenA(
          oidc.isEmpty
        )(Log.println("[skakapo] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")))
        _ <- Resource.eval(IO.whenA(
          box.isEmpty
        )(Log.println("[skakapo] WARNING: APP_SECRET が未設定のため、各サービスとの連携（トークンの保存）を使えません")))
        _ <- Resource.eval(IO.whenA(
          !cfg.api.agentReady
        )(Log.println("[skakapo] WARNING: ANTHROPIC_API_KEY が未設定のため、エージェントを動かせません")))
        // OIDC の設定の誤り（issuer の URL など）は起動時に知らせる（IdP が止まっていても起動はする）
        _ <- Resource.eval(oidc.traverse_(_.discover.attempt.flatMap {
          case Right(m) => Log.println(s"[skakapo] OIDC: ${m.issuer}")
          case Left(e) => Log.println(
              s"[skakapo] WARNING: OIDC の discovery に失敗しました（${e.getMessage}）。ログインのときにもう一度試します"
            )
        }))
        // プッシュ通知（PWA）: エージェントが返事をした・失敗した
        pushStore = new PushStore(xa, keepOwner = true)
        push <- Resource.eval(PushService.create(pushStore, http, cfg.api.publicUrl, "skakapo"))
        creditStore = new CreditStore(xa)
        credits = new Credits(creditStore, config)
        hosts = new GitHosts(http)
        clouds = new CloudClients(http)
        runner = new Runner(
          store,
          users,
          config,
          conns,
          Claude.fromEnv,
          mcp,
          hosts,
          Some(push),
          workBase,
          credits,
          box,
          clouds
        )
        _ <- purgeLoop(users, connStore, store, workBase).background
        // 実行を取り合う（WORKER_SLOTS=0 のノードは画面と API だけ）
        _ <- if cfg.slots > 0 then new WorkerPool(store, nodes, runner, PoolConfig(cfg.node, version, cfg.slots)).run
        else Resource.unit[IO]
        web <- Resource.eval(cfg.staticDir.traverse(spa(_, cfg.oidc.flatMap(o => idpOrigin(o.issuer)))))
        routes = new SessionRoutes(store, config, sess, workBase, credits).routes <+>
          new ConnectionRoutes(conns, connStore, config, sess).routes <+>
          new AdminRoutes(
            config,
            users,
            nodes,
            runner,
            conns,
            hosts,
            clouds,
            box,
            cfg.api.agentReady,
            sess,
            creditStore
          ).routes
        all = ApiRoutes(oidc, users, sess, cfg.api, routes, Some(pushStore -> push)) <+>
          healthRoutes(xa) <+> RumRoutes.routes <+> web.getOrElse(HttpRoutes.empty[IO])
        // セキュリティのヘッダーは、見つからない 404・内部エラーの 500 にも付ける
        httpApp <- HttpTelemetry(
          SecurityHeaders.app(Errors.handle(all.orNotFound, ErrorCode.Unexpected), cfg.api.cookieSecure),
          List("/api", "/auth", "/mcp"),
          sbirds.http.RateLimit.clientIp(_, cfg.api.rateLimit.trustedProxies)
        )
        _ <- EmberServerBuilder.default[IO]
          .withHost(cfg.bindHost)
          .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
          .withShutdownTimeout(5.seconds)
          .withHttpApp(httpApp)
          .build
        _ <- Resource.eval(Log.println(
          s"[skakapo] listening on :${cfg.port} (${cfg.api.publicUrl}, node ${cfg.node}, slots ${cfg.slots})"
        ))
      } yield ()).useForever
    } yield ()

  /** DB が使えるまで待ってからマイグレーションする. */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else
        Log.report(ErrorCode.DatabaseNotReady, s"${e.getMessage.take(120)}; retry $attempt/60") *>
          IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  private def healthRoutes(xa: doobie.Transactor[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select 1".query[Int].unique.transact(xa).timeout(3.seconds).attempt.flatMap {
        case Right(_) => IO.pure(TextResponse(Status.Ok, "ok"))
        case Left(e) => Log.report(
            ErrorCode.HealthCheckFailed,
            "",
            e
          ).as(TextResponse(Status.ServiceUnavailable, "database unavailable"))
      }
  }

  /** DB から定期的に読むメトリクス（skorimako のダッシュボード・アラート用）. */
  private def gauges(xa: doobie.Transactor[IO]): Resource[IO, Unit] = {
    def q(sqlText: doobie.Fragment): IO[Long] = sqlText.query[Long].unique.transact(xa)
    List(
      Metrics.gauge("skakapo.users.total", "有効なユーザーの数")(q(sql"select count(*) from app_user where not disabled")),
      Metrics.gauge(
        "skakapo.users.active_24h",
        "24時間以内にログインしたユーザーの数"
      )(q(sql"select count(*) from app_user where last_login_at > now() - interval '24 hours'")),
      Metrics.gauge("skakapo.sessions.total", "会話の数")(q(sql"select count(*) from agent_session")),
      Metrics.gauge(
        "skakapo.runs.queued",
        "順番を待っている実行の数",
        15.seconds
      )(q(sql"select count(*) from run where status = 'queued'")),
      Metrics.gauge(
        "skakapo.runs.running",
        "動いている実行の数",
        15.seconds
      )(q(sql"select count(*) from run where status = 'running'")),
      Metrics.gauge(
        "skakapo.runs.failed_24h",
        "24時間以内に失敗した実行の数"
      )(q(sql"select count(*) from run where status = 'failed' and finished_at > now() - interval '24 hours'")),
      Metrics.gauge("skakapo.connections.total", "利用者が連携しているサービスの数")(q(sql"select count(*) from connection")),
      Metrics.gauge("skakapo.nodes.alive", "動いているノードの数", 30.seconds)(q(fr"select count(*) from node where seen_at >" ++
        Sql.AliveWindow)),
      Metrics.gauge("skakapo.nodes.free_slots", "動いているノードの空きの数（足りなければノードを足す）", 15.seconds)(
        q(fr"select coalesce(sum(greatest(slots - active, 0)), 0)::bigint from node where seen_at >" ++ Sql.AliveWindow)
      ),
      Metrics.gauge("sbirds.push.subscriptions", "プッシュ通知を受け取る端末の数")(q(sql"select count(*) from push_subscription"))
    ).sequence_
  }

  /** 期限の切れたセッション・連携の途中の状態・会話に使われなかった画像・7日使っていない作業場所を消す（1時間ごと）. */
  private def purgeLoop(users: UserStore,
                        conns: ConnectionStore,
                        sessions: SessionStore,
                        workBase: java.nio.file.Path): IO[Unit] =
    Periodic.run(1.minute, 1.hour, ErrorCode.PurgeFailed)(
      IO.realTimeInstant.flatMap(users.purgeExpired) *> conns.purge *> sessions.purgeAttachments *>
        purgeWorkspaces(workBase)
    ).void

  private def purgeWorkspaces(base: java.nio.file.Path): IO[Unit] = IO.blocking {
    val dir = base.resolve("sessions")
    if java.nio.file.Files.isDirectory(dir) then {
      val old = java.time.Instant.now().minus(java.time.Duration.ofDays(7))
      val s = java.nio.file.Files.list(dir)
      try s.iterator().asScala.filter(p => java.nio.file.Files.getLastModifiedTime(p).toInstant.isBefore(old)).toList
      finally s.close()
    } else Nil
  }.flatMap(_.traverse_(p => Workspace(p).remove))

  /**
   * 画面の CSP. スクリプトは自分のサイトのものと、index.html に直接書いたもの（テーマの読み込み. ハッシュで許す）だけ.
   * skorimako のブラウザの観測（RUM）が設定されていれば、その rum.js の読み込みと送信も許す（rum はそのオリジン）.
   * 画像はアイコン（IdP の画像は外部の https）のため広めにする. ヘッダーのロゴは IdP（sruru）が配るので、
   * http の開発環境でも出るよう IdP のオリジンも足す. Service Worker・manifest は自分のサイトなので default-src で足りる.
   */
  def spaCsp(indexHtml: String, rum: Option[String] = RumRoutes.origin, idp: Option[String] = None): String = {
    val inline = "(?s)<script>(.*?)</script>".r.findAllMatchIn(indexHtml).map(m =>
      s"'sha256-${java.util.Base64.getEncoder.encodeToString(java.security.MessageDigest.getInstance("SHA-256").digest(m.group(1).getBytes("UTF-8")))}'"
    )
    (List(
      "default-src 'self'",
      ("script-src 'self'" :: inline.toList ++ rum.toList).mkString(" "),
      "style-src 'self' 'unsafe-inline'",
      ("img-src 'self' data: blob: https:" :: idp.toList).mkString(" "),
      ("connect-src 'self'" :: rum.toList).mkString(" "),
      "object-src 'none'",
      "base-uri 'self'",
      "frame-ancestors 'none'",
      "form-action 'self'"
    )).mkString("; ")
  }

  /** ビルド済みの画面の配信. 存在しないパスは index.html（SPA）. */
  /** IdP の issuer の URL のオリジン（例: http://localhost:8081）. */
  def idpOrigin(issuer: String): Option[String] = scala.util.Try(java.net.URI.create(issuer)).toOption
    .filter(u => u.getScheme != null && u.getHost != null)
    .map(u => s"${u.getScheme}://${u.getHost}" + (if u.getPort >= 0 then s":${u.getPort}" else ""))

  private def spa(dir: String, idp: Option[String]): IO[HttpRoutes[IO]] =
    IO.blocking(java.nio.file.Files.readString(java.nio.file.Path.of(dir, "index.html"))).map(spaRoutes(dir, _, idp))

  private def spaRoutes(dir: String, indexHtml: String, idp: Option[String]): HttpRoutes[IO] = {
    val csp = spaCsp(indexHtml, idp = idp)
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> path
          if !path.segments.headOption.map(_.decoded()).exists(Set("api", "auth", "mcp", "healthz")) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(Errors.respond(ErrorCode.PageNotFound))
    }
    HttpRoutes[IO] { req =>
      val cc =
        if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc, "Content-Security-Policy" -> csp))
    }
  }
}

package skakapo.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling
import org.http4s.server.staticcontent.*
import skakapo.agent.*
import skakapo.api.*
import skakapo.auth.{OidcClient, OidcConfig, SecretBox}
import skakapo.db.*
import skakapo.observe.{HttpClientTelemetry, HttpTelemetry, Metrics, RumRoutes, Telemetry}
import skakapo.push.{PushService, PushStore}
import skakapo.shared.ErrorDto

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

final case class AppConfig(port: Int, db: DbConfig, oidc: Option[OidcConfig], api: ApiConfig, staticDir: Option[String],
    appSecret: Option[String], node: String, slots: Int, workDir: String, bindHost: com.comcast.ip4s.Host)

/** 設定は環境変数から読む. */
object Config:
  private def opt(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)
  private def int(name: String): Option[Int]    = opt(name).flatMap(_.toIntOption)
  private def enabled(name: String): Boolean    = opt(name).exists(_.equalsIgnoreCase("true"))
  private def required(name: String): Either[String, String] = opt(name).toRight(s"環境変数 $name を設定してください")
  private def list(name: String): List[String] = opt(name).toList.flatMap(_.split(Array(',', ' ')).map(_.trim).filter(_.nonEmpty))

  private def local(url: String): Boolean =
    scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)
      .exists(h => Set("localhost", "127.0.0.1", "[::1]").contains(h) || h.endsWith(".localhost"))

  /** ノードの名前（既定はホスト名. コンテナではコンテナの ID になる）. */
  private def hostName: String = scala.util.Try(java.net.InetAddress.getLocalHost.getHostName).getOrElse("node")

  def load: IO[AppConfig] = IO.fromEither(
    (for
      dbUrl  <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub    <- required("PUBLIC_URL").map(_.stripSuffix("/"))
      devLogin = enabled("DEV_LOGIN")
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match
                // グループを絞らないと IdP のだれでもログインでき、エージェント（Claude API の費用・各サービスへの操作）を使える. 明示したときだけ許す
                case (Some(_), Some(_), Some(_)) if list("OIDC_ALLOWED_GROUPS").isEmpty && !enabled("OIDC_ALLOW_ANY_USER") =>
                  Left("OIDC_ALLOWED_GROUPS でログインできるグループを絞ってください（IdP のだれでもログインしてよいときは OIDC_ALLOW_ANY_USER=true）")
                case (Some(i), Some(c), Some(s)) =>
                  Right(Some(OidcConfig(i, c, s, s"$pub/auth/callback", opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
                    Some(list("OIDC_SCOPES")).filter(_.nonEmpty).getOrElse(List("openid", "profile", "email")), list("OIDC_ALLOWED_GROUPS").toSet)))
                // 開発用のログインは名前だけでだれにでもなれるので、手元（localhost）で開くときだけ
                case (None, None, None) if devLogin && !local(pub) => Left("DEV_LOGIN は PUBLIC_URL が localhost のときだけ使えます")
                case (None, None, None) if devLogin => Right(None)
                case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      proxies <- list("TRUSTED_PROXIES").traverse(s => RateLimitConfig.proxy(s).toRight(s"TRUSTED_PROXIES の「$s」は IP か CIDR で書いてください"))
      secret <- opt("APP_SECRET") match
                  case Some(s) if s.length < 16 => Left("APP_SECRET は16文字以上にしてください")
                  case s                        => Right(s)
      // このノードで同時に動かす実行の数（エージェントは重いので、マシンの大きさに合わせる）
      slots <- int("WORKER_SLOTS").getOrElse(2) match
                case n if n >= 0 && n <= 64 => Right(n)
                case _                      => Left("WORKER_SLOTS は 0〜64 にしてください（0 は画面と API だけのノード）")
      // 待ち受けるアドレス（既定 0.0.0.0. 開発用のログインのときは 127.0.0.1）
      bind   <- opt("BIND_HOST").fold[Either[String, com.comcast.ip4s.Host]](Right(if devLogin then ipv4"127.0.0.1" else ipv4"0.0.0.0"))(h =>
                  com.comcast.ip4s.Host.fromString(h).toRight("BIND_HOST は IP アドレスかホスト名で書いてください"))
      // 開発用のログイン（名前だけで管理者にもなれる）は、手元からしか届かないようにする
      // （コンテナーの中でロードバランサーの後ろに置くときなどは、DEV_LOGIN_ALLOW_REMOTE=true で明示する）
      _      <- Either.cond(!devLogin || oidc.isDefined || opt("DEV_LOGIN_ALLOW_REMOTE").contains("true") ||
                  Set("127.0.0.1", "::1", "localhost").contains(bind.toString), (),
                  "DEV_LOGIN のときは BIND_HOST を 127.0.0.1 にしてください（開発用のログインを外に出さないため。" +
                  "ロードバランサーの後ろに置くなど、外から直接は届かないなら DEV_LOGIN_ALLOW_REMOTE=true）")
    yield AppConfig(
      port = int("HTTP_PORT").getOrElse(8080),
      db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(slots * 2 + 8)),
      oidc = oidc,
      api = ApiConfig(cookieSecure = pub.startsWith("https://"), publicUrl = pub, sessionTtl = int("SESSION_DAYS").getOrElse(14).days,
        devLogin = devLogin, adminGroups = list("ADMIN_GROUPS").toSet, agentReady = sys.env.get("ANTHROPIC_API_KEY").exists(_.trim.nonEmpty),
        rateLimit = RateLimitConfig(int("RATE_LIMIT_AUTH").getOrElse(20), int("RATE_LIMIT_PUBLIC").getOrElse(120), proxies)),
      staticDir = opt("STATIC_DIR"),
      appSecret = secret,
      node = opt("NODE_NAME").getOrElse(hostName).take(100),
      slots = slots,
      workDir = opt("WORK_DIR").getOrElse("work"),
      bindHost = bind
    )).left.map(new IllegalArgumentException(_))
  )

object Main extends IOApp.Simple:

  // ログにトレースの ID・操作したユーザーを載せるため、IOLocal の文脈を Java のライブラリから見えるようにする
  // （IORuntime を作る前に設定する必要がある. オブジェクトの初期化は main より先に動く）
  Telemetry.prepare()

  private val version = Option(getClass.getPackage.getImplementationVersion).orElse(sys.env.get("APP_VERSION")).getOrElse("dev")

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for
      cfg <- Config.load
      n   <- migrateWithRetry(cfg)
      _   <- skakapo.observe.Log.println(s"[skakapo] database migrated ($n migration(s) applied)")
      _ <- (for
        _    <- Telemetry.resource("skakapo", version, runtime)
        xa   <- Database.transactor(cfg.db)
        _    <- gauges(xa)
        // 外への呼び出し（IdP・通知先・プッシュのサービス）もトレースに出す（traceparent を付けて送る）
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build.map(HttpClientTelemetry(_))
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        users     = new UserStore(xa)
        sess      = new SessionAuth(users, cfg.api)
        box       = cfg.appSecret.map(new SecretBox(_))
        config    = new ConfigStore(xa)
        connStore = new ConnectionStore(xa)
        store     = new SessionStore(xa)
        nodes     = new NodeStore(xa)
        mcp       = new Mcp(http)
        conns     = new Connections(config, connStore, box, http, mcp, cfg.api.publicUrl)
        workBase  = java.nio.file.Path.of(cfg.workDir).toAbsolutePath.normalize()
        _ <- Resource.eval(config.seedDefaults)
        _ <- Resource.eval(IO.whenA(oidc.isEmpty)(skakapo.observe.Log.println("[skakapo] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")))
        _ <- Resource.eval(IO.whenA(box.isEmpty)(skakapo.observe.Log.println("[skakapo] WARNING: APP_SECRET が未設定のため、各サービスとの連携（トークンの保存）を使えません")))
        _ <- Resource.eval(IO.whenA(!cfg.api.agentReady)(skakapo.observe.Log.println("[skakapo] WARNING: ANTHROPIC_API_KEY が未設定のため、エージェントを動かせません")))
        // OIDC の設定の誤り（issuer の URL など）は起動時に知らせる（IdP が止まっていても起動はする）
        _ <- Resource.eval(oidc.traverse_(_.discover.attempt.flatMap {
               case Right(m) => skakapo.observe.Log.println(s"[skakapo] OIDC: ${m.issuer}")
               case Left(e)  => skakapo.observe.Log.println(s"[skakapo] WARNING: OIDC の discovery に失敗しました（${e.getMessage}）。ログインのときにもう一度試します")
             }))
        // プッシュ通知（PWA）: エージェントが返事をした・失敗した
        pushStore = new PushStore(xa)
        push     <- Resource.eval(PushService.create(pushStore, http, cfg.api.publicUrl, "skakapo"))
        creditStore = new CreditStore(xa)
        credits   = new Credits(creditStore, config)
        hosts     = new GitHosts(http)
        clouds    = new CloudClients(http)
        runner    = new Runner(store, users, config, conns, Claude.fromEnv, mcp, hosts, Some(push), workBase, credits, box, clouds)
        _ <- purgeLoop(users, connStore, store, workBase).background
        // 実行を取り合う（WORKER_SLOTS=0 のノードは画面と API だけ）
        _ <- if cfg.slots > 0 then new WorkerPool(store, nodes, runner, PoolConfig(cfg.node, version, cfg.slots)).run else Resource.unit[IO]
        web <- Resource.eval(cfg.staticDir.traverse(spa))
        routes = new SessionRoutes(store, config, sess, workBase, credits).routes <+> new ConnectionRoutes(conns, connStore, config, sess).routes <+>
                   new AdminRoutes(config, users, nodes, runner, conns, hosts, clouds, box, cfg.api.agentReady, sess, creditStore).routes
        all = ApiRoutes(oidc, users, sess, cfg.api, routes, Some(pushStore -> push)) <+>
                healthRoutes(xa) <+> RumRoutes.routes <+> web.getOrElse(HttpRoutes.empty[IO])
        // セキュリティのヘッダーは、見つからない 404・内部エラーの 500 にも付ける
        httpApp <- HttpTelemetry(SecurityHeaders.app(ErrorHandling.Custom.recoverWith(all.orNotFound)(handler), cfg.api.cookieSecure),
                     List("/api", "/auth", "/mcp"), RateLimit.clientIp(_, cfg.api.rateLimit.trustedProxies))
        _ <- EmberServerBuilder.default[IO]
               .withHost(cfg.bindHost)
               .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
               .withShutdownTimeout(5.seconds)
               .withHttpApp(httpApp)
               .build
        _ <- Resource.eval(skakapo.observe.Log.println(s"[skakapo] listening on :${cfg.port} (${cfg.api.publicUrl}, node ${cfg.node}, slots ${cfg.slots})"))
      yield ()).useForever
    yield ()

  /** DB が使えるまで待ってからマイグレーションする. */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else skakapo.observe.Log.println(s"[skakapo] database not ready (${e.getMessage.take(120)}); retry $attempt/60") *> IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  private def healthRoutes(xa: doobie.Transactor[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select 1".query[Int].unique.transact(xa).timeout(3.seconds).attempt.map {
        case Right(_) => TextResponse(Status.Ok, "ok")
        case Left(_)  => TextResponse(Status.ServiceUnavailable, "database unavailable")
      }
  }

  private val handler: PartialFunction[Throwable, IO[Response[IO]]] = { case e =>
    IO(org.slf4j.LoggerFactory.getLogger("skakapo").error("unhandled error", e)) *> InternalServerError(ErrorDto("サーバーでエラーが発生しました"))
  }

  /** DB から定期的に読むメトリクス（skorimako のダッシュボード・アラート用）. */
  private def gauges(xa: doobie.Transactor[IO]): Resource[IO, Unit] =
    def q(sqlText: doobie.Fragment): IO[Long] = sqlText.query[Long].unique.transact(xa)
    List(
      Metrics.gauge("skakapo.users.total", "有効なユーザーの数")(q(sql"select count(*) from app_user where not disabled")),
      Metrics.gauge("skakapo.users.active_24h", "24時間以内にログインしたユーザーの数")(q(sql"select count(*) from app_user where last_login_at > now() - interval '24 hours'")),
      Metrics.gauge("skakapo.sessions.total", "会話の数")(q(sql"select count(*) from agent_session")),
      Metrics.gauge("skakapo.runs.queued", "順番を待っている実行の数", 15.seconds)(q(sql"select count(*) from run where status = 'queued'")),
      Metrics.gauge("skakapo.runs.running", "動いている実行の数", 15.seconds)(q(sql"select count(*) from run where status = 'running'")),
      Metrics.gauge("skakapo.runs.failed_24h", "24時間以内に失敗した実行の数")(q(sql"select count(*) from run where status = 'failed' and finished_at > now() - interval '24 hours'")),
      Metrics.gauge("skakapo.connections.total", "利用者が連携しているサービスの数")(q(sql"select count(*) from connection")),
      Metrics.gauge("skakapo.nodes.alive", "動いているノードの数", 30.seconds)(q(fr"select count(*) from node where seen_at >" ++ Sql.AliveWindow)),
      Metrics.gauge("skakapo.nodes.free_slots", "動いているノードの空きの数（足りなければノードを足す）", 15.seconds)(
        q(fr"select coalesce(sum(greatest(slots - active, 0)), 0)::bigint from node where seen_at >" ++ Sql.AliveWindow)),
      Metrics.gauge("skakapo.push.subscriptions", "プッシュ通知を受け取る端末の数")(q(sql"select count(*) from push_subscription"))
    ).sequence_

  /** 期限の切れたセッション・連携の途中の状態・会話に使われなかった画像・7日使っていない作業場所を消す（1時間ごと）. */
  private def purgeLoop(users: UserStore, conns: ConnectionStore, sessions: SessionStore, workBase: java.nio.file.Path): IO[Unit] =
    (IO.sleep(1.minute) *> ((IO.realTimeInstant.flatMap(users.purgeExpired) *> conns.purge *> sessions.purgeAttachments *> purgeWorkspaces(workBase))
      .handleErrorWith(e => skakapo.observe.Log.println(s"[skakapo] purge failed: $e")) *> IO.sleep(1.hour)).foreverM).void

  private def purgeWorkspaces(base: java.nio.file.Path): IO[Unit] = IO.blocking {
    val dir = base.resolve("sessions")
    if java.nio.file.Files.isDirectory(dir) then
      val old = java.time.Instant.now().minus(java.time.Duration.ofDays(7))
      val s = java.nio.file.Files.list(dir)
      try s.iterator().asScala.filter(p => java.nio.file.Files.getLastModifiedTime(p).toInstant.isBefore(old)).toList
      finally s.close()
    else Nil
  }.flatMap(_.traverse_(p => Workspace(p).remove))

  /**
   * 画面の CSP. スクリプトは自分のサイトのものと、index.html に直接書いたもの（テーマの読み込み. ハッシュで許す）だけ.
   * skorimako のブラウザの観測（RUM）が設定されていれば、その rum.js の読み込みと送信も許す（rum はそのオリジン）.
   * 画像はアイコン（IdP の画像は外部の https）のため広めにする. Service Worker・manifest は自分のサイトなので default-src で足りる.
   */
  def spaCsp(indexHtml: String, rum: Option[String] = RumRoutes.origin): String =
    val inline = "(?s)<script>(.*?)</script>".r.findAllMatchIn(indexHtml).map(m =>
      s"'sha256-${java.util.Base64.getEncoder.encodeToString(java.security.MessageDigest.getInstance("SHA-256").digest(m.group(1).getBytes("UTF-8")))}'")
    (List("default-src 'self'", ("script-src 'self'" :: inline.toList ++ rum.toList).mkString(" "), "style-src 'self' 'unsafe-inline'",
      "img-src 'self' data: blob: https:", ("connect-src 'self'" :: rum.toList).mkString(" "), "object-src 'none'", "base-uri 'self'",
      "frame-ancestors 'none'", "form-action 'self'")).mkString("; ")

  /** ビルド済みの画面の配信. 存在しないパスは index.html（SPA）. */
  private def spa(dir: String): IO[HttpRoutes[IO]] =
    IO.blocking(java.nio.file.Files.readString(java.nio.file.Path.of(dir, "index.html"))).map(spaRoutes(dir, _))

  private def spaRoutes(dir: String, indexHtml: String): HttpRoutes[IO] =
    val csp   = spaCsp(indexHtml)
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> path if !path.segments.headOption.map(_.decoded()).exists(Set("api", "auth", "mcp", "healthz")) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(NotFound())
    }
    HttpRoutes[IO] { req =>
      val cc = if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc, "Content-Security-Policy" -> csp))
    }

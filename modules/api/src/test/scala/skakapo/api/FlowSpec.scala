package skakapo.api

import cats.effect.*
import cats.syntax.all.*
import com.anthropic.core.ObjectMappers
import com.anthropic.models.messages.{Message, MessageParam, TextBlockParam, ToolUnion}
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{Authorization, Cookie}
import org.http4s.implicits.*
import skakapo.agent.*
import skakapo.auth.SecretBox
import skakapo.db.*
import skakapo.shared.*

import doobie.implicits.*
import doobie.postgres.implicits.*
import java.nio.file.Files
import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/**
 * 管理者の設定 → 利用者の OAuth の連携（ツールを覚える） → フォームで依頼 → ノードが実行を取り、エージェントが MCP のツールを呼んで返事をする、
 * までを通しで確かめる. Claude API は、決まった応答を返す偽物に差し替える（MCP のサービスと OAuth の入口も偽物）.
 * DB は docker-compose.dev.yml（:54410）. TEST_DATABASE_URL で変えられる.
 */
class FlowSpec extends munit.CatsEffectSuite {

  override def munitIOTimeout: Duration = 90.seconds

  private val dbCfg = DbConfig(
    sys.env.getOrElse("TEST_DATABASE_URL", "jdbc:postgresql://localhost:54410/skakapo_test"),
    "skakapo",
    "skakapo",
    6
  )
  private val apiCfg =
    ApiConfig(cookieSecure = false, publicUrl = "http://localhost:8120", devLogin = true, agentReady = true)

  /** 偽の skea（OAuth のトークンの発行と MCP）. 受けた MCP の呼び出し（ツール, 引数, トークン）を残す. */
  private val calls = new ConcurrentLinkedQueue[(String, Json, String)]()
  private val fakeSkea: HttpRoutes[IO] = {
    import org.http4s.dsl.io.*
    HttpRoutes.of[IO] {
      case r @ POST -> Root / "oauth" / "token" =>
        r.as[UrlForm].flatMap { f =>
          val basic = r.headers.get[Authorization].map(_.credentials.toString).getOrElse("")
          if f.getFirstOrElse("code", "") == "good-code" && basic.startsWith("Basic ") &&
            f.getFirst("code_verifier").exists(_.length >= 43)
          then
            Ok(Json.obj(
              "access_token" -> "skea_user_token".asJson,
              "token_type" -> "Bearer".asJson,
              "scope" -> "read write".asJson
            ))
          else BadRequest(Json.obj("error" -> "invalid_grant".asJson))
        }
      case r @ POST -> Root / "mcp" =>
        val token = r.headers.get[Authorization].map(_.credentials.toString.stripPrefix("Bearer ")).getOrElse("")
        if token != "skea_user_token" && token != "skea_agent_token" then Forbidden(Json.obj("error" -> "no".asJson))
        else
          r.as[Json].flatMap { j =>
            val c = j.hcursor
            val id = c.downField("id").focus.getOrElse(Json.Null)
            def ok(res: Json) = Ok(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "result" -> res))
            c.get[String]("method").getOrElse("") match {
              case "initialize" =>
                ok(Json.obj("protocolVersion" -> "2025-06-18".asJson, "capabilities" -> Json.obj()))
              case "notifications/initialized" => Accepted()
              case "tools/list" => ok(Json.obj("tools" -> Json.arr(
                  Json.obj(
                    "name" -> "create_task".asJson,
                    "description" -> "タスクを作る".asJson,
                    "inputSchema" -> Json.obj(
                      "type" -> "object".asJson,
                      "properties" -> Json.obj("title" -> Json.obj("type" -> "string".asJson)),
                      "required" -> List("title").asJson
                    )
                  ),
                  Json.obj(
                    "name" -> "list_tasks".asJson,
                    "description" -> "タスクの一覧".asJson,
                    "annotations" -> Json.obj("readOnlyHint" -> true.asJson),
                    "inputSchema" -> Json.obj("type" -> "object".asJson)
                  )
                )))
              case "tools/call" =>
                val name = c.downField("params").get[String]("name").getOrElse("")
                val args = c.downField("params").downField("arguments").focus.getOrElse(Json.obj())
                IO(calls.add((name, args, token))) *>
                  ok(Json.obj("content" -> Json.arr(Json.obj(
                    "type" -> "text".asJson,
                    "text" -> "SKEA-7 を作りました: https://skea.test/t/SKEA-7".asJson
                  ))))
              case m => ok(Json.obj("unknown" -> m.asJson))
            }
          }
    }
  }

  /** 決まった応答を返す偽の Claude. 受けたシステムプロンプトとツールの名前を残す. */
  private final class FakeLlm(script: List[String]) extends Llm {
    val systems = new ConcurrentLinkedQueue[String]()
    val toolNames = new ConcurrentLinkedQueue[List[String]]()
    private val step = Ref.unsafe[IO, Int](0)
    def turn(model: String,
             maxTokens: Int,
             effort: String,
             fallbacks: Boolean,
             system: List[TextBlockParam],
             tools: List[ToolUnion],
             messages: List[MessageParam]): IO[Turn] =
      step.getAndUpdate(_ + 1).map { i =>
        systems.add(system.map(_.text()).mkString("\n"))
        val tj = parse(ObjectMappers.jsonMapper().writeValueAsString(tools.asJava)).toOption.get
        toolNames.add(tj.asArray.toList.flatten.flatMap(_.hcursor.get[String]("name").toOption))
        val m = ObjectMappers.jsonMapper().readValue(script(i.min(script.size - 1)), classOf[Message])
        Turn(m, 100, 20, 80)
      }
  }

  private def msg(stop: String, content: String): String =
    s"""{"id":"msg_x","type":"message","role":"assistant","model":"claude-opus-5-5","content":$content,"stop_reason":"$stop","stop_sequence":null,
       |"usage":{"input_tokens":100,"output_tokens":20}}""".stripMargin

  private val llm = new FakeLlm(List(
    msg("tool_use", """[{"type":"tool_use","id":"toolu_0","name":"set_title","input":{"title":"期限の並び順の不具合"}}]"""),
    msg(
      "tool_use",
      """[{"type":"text","text":"起票します"},{"type":"tool_use","id":"toolu_1","name":"skea__create_task","input":{"title":"並び順"}}]"""
    ),
    msg(
      "tool_use",
      """[{"type":"tool_use","id":"toolu_2","name":"record_link","input":{"label":"SKEA-7 の不具合","url":"https://skea.test/t/SKEA-7"}}]"""
    ),
    msg("end_turn", """[{"type":"text","text":"SKEA-7 を起票しました。"}]""")
  ))

  private final case class Env(app: HttpApp[IO], xa: doobie.Transactor[IO])

  private val fixture = ResourceSuiteLocalFixture(
    "app",
    for {
      _ <- Resource.eval(IO.blocking {
        val c = java.sql.DriverManager.getConnection(dbCfg.url, dbCfg.user, dbCfg.password)
        try c.createStatement().execute("drop schema public cascade; create schema public")
        finally c.close()
      })
      _ <- Resource.eval(Database.migrate(dbCfg))
      xa <- Database.transactor(dbCfg)
      users = new UserStore(xa); config = new ConfigStore(xa); connStore = new ConnectionStore(xa);
      store = new SessionStore(xa); nodes = new NodeStore(xa)
      _ <- Resource.eval(config.seedDefaults)
      http = Client.fromHttpApp(fakeSkea.orNotFound)
      box = Some(SecretBox("test-app-secret-0123456789"))
      mcp = new Mcp(http)
      conns = new Connections(config, connStore, box, http, mcp, apiCfg.publicUrl)
      work = Files.createTempDirectory("skakapo-flow")
      sess = new SessionAuth(users, apiCfg)
      creditStore = new CreditStore(xa)
      credits = new Credits(creditStore, config)
      gitHosts = new GitHosts(http)
      cloudClients = new CloudClients(http)
      runner = new Runner(store, users, config, conns, Some(llm), mcp, gitHosts, None, work, credits, box, cloudClients)
      _ <- new WorkerPool(store, nodes, runner, PoolConfig("node-a", "test", slots = 2)).run
      routes = new SessionRoutes(store, config, sess, work, credits).routes <+>
        new ConnectionRoutes(conns, connStore, config, sess).routes <+>
        new AdminRoutes(
          config,
          users,
          nodes,
          runner,
          conns,
          gitHosts,
          cloudClients,
          box,
          true,
          sess,
          creditStore
        ).routes
    } yield Env(ApiRoutes(None, users, sess, apiCfg, routes).orNotFound, xa)
  )

  override def munitFixtures = List(fixture)

  private def login(name: String, admin: Boolean): IO[String] =
    fixture().app.run(Request[IO](
      Method.POST,
      uri"/auth/dev-login"
    ).withEntity(Json.obj("name" -> name.asJson, "admin" -> admin.asJson))
      .putHeaders("X-Skakapo" -> "1")).map(r =>
      r.cookies.find(_.name == "skakapo_session").map(_.content).getOrElse(fail("no cookie"))
    )

  private def call(ck: String, m: Method, path: String, body: Option[Json] = None): IO[Response[IO]] = {
    val r = Request[IO](
      m,
      Uri.unsafeFromString(path)
    ).putHeaders(Cookie(RequestCookie("skakapo_session", ck)), "X-Skakapo" -> "1")
    fixture().app.run(body.fold(r)(r.withEntity(_)))
  }

  private def json(ck: String,
                   m: Method,
                   path: String,
                   body: Option[Json] = None,
                   expect: Status = Status.Ok): IO[Json] =
    call(ck, m, path, body).flatMap(r =>
      r.as[Json].handleError(_ => Json.Null).map { j => assertEquals(r.status, expect, j.noSpaces); j }
    )

  test("管理者がサービスを設定し、利用者が OAuth で連携してフォームで依頼すると、エージェントが MCP のツールを使って返事をする") {
    for {
      admin <- login("管理者", admin = true)
      user <- login("山田", admin = false)
      // モデルは一覧から選ぶ
      _ <- json(
        admin,
        Method.PUT,
        "/api/admin/settings",
        Some(AdminSettingsReq(AgentSettings(model = "gpt-x"), List(TrackerSettings())).asJson),
        Status.BadRequest
      )
      // 起票先は複数. 既定は1つにそろえる
      ts <- json(
        admin,
        Method.PUT,
        "/api/admin/settings",
        Some(AdminSettingsReq(
          AgentSettings(),
          List(
            TrackerSettings("a", "skea", "A", isDefault = false),
            TrackerSettings("b", "skea", "B", isDefault = false)
          )
        ).asJson)
      )
      _ = assertEquals(
        ts.hcursor.downField("trackers").as[List[TrackerSettings]].map(_.map(t => (t.name, t.isDefault))),
        Right(List(("a", true), ("b", false)))
      )
      // 一般の利用者は管理画面を使えない
      _ <- call(user, Method.GET, "/api/admin/settings").map(r => assertEquals(r.status, Status.Forbidden))
      // 管理者: 前提のコンテキストと、skea の URL・MCP・OAuth のクライアント
      _ <- json(
        admin,
        Method.PUT,
        "/api/admin/settings",
        Some(AdminSettingsReq(
          AgentSettings(baseContext = "必ず丁寧語で答える"),
          List(TrackerSettings("社内", "skea", "SKEA"))
        ).asJson)
      )
      svcs <- json(admin, Method.GET, "/api/admin/services").map(_.as[List[ServiceDto]].toOption.get)
      skea = svcs.find(_.key == "skea").get
      _ <- json(
        admin,
        Method.PUT,
        s"/api/admin/services/${skea.id}",
        Some(ServiceReq(
          "skea",
          "skea",
          ServiceKind.Sbirds,
          "http://skea.test",
          "http://skea.test/mcp",
          "プロジェクト管理",
          "起票は SKEA へ",
          OAuthConfig("http://skea.test/oauth/authorize", "http://skea.test/oauth/token", "skakapo", "read write"),
          Some("skakapo-secret-0123456789"),
          enabled = true,
          position = 1
        ).asJson)
      )
      // 利用者: OAuth を始めて、skea から戻ってきたところ（認可コード）
      cs <- json(user, Method.GET, "/api/connections").map(_.as[List[ConnectionDto]].toOption.get)
      _ = assert(cs.find(_.service.key == "skea").exists(c => c.canOAuth && !c.connected), cs)
      start <- json(user, Method.POST, s"/api/connections/${skea.id}/oauth")
      authUrl = Uri.unsafeFromString(start.hcursor.get[String]("url").toOption.get)
      _ = assertEquals(authUrl.params.get("redirect_uri"), Some("http://localhost:8120/api/connections/callback"))
      _ = assertEquals(authUrl.params.get("code_challenge_method"), Some("S256"))
      state = authUrl.params("state")
      // 別の人は、その連携の続きを使えない
      other <- login("佐藤", admin = false)
      bad <- call(other, Method.GET, s"/api/connections/callback?state=$state&code=good-code")
      _ = assertEquals(bad.status, Status.BadRequest)
      start2 <- json(user, Method.POST, s"/api/connections/${skea.id}/oauth")
      state2 = Uri.unsafeFromString(start2.hcursor.get[String]("url").toOption.get).params("state")
      cb <- call(user, Method.GET, s"/api/connections/callback?state=$state2&code=good-code")
      _ = assertEquals(cb.status, Status.Found)
      cs2 <- json(user, Method.GET, "/api/connections").map(_.as[List[ConnectionDto]].toOption.get)
      _ = assert(cs2.find(_.service.key == "skea").exists(_.connected))
      // 連携したときに、skea のツールをシステムとして覚えた（管理画面と前提に出る）
      svcs2 <- json(admin, Method.GET, "/api/admin/services").map(_.as[List[ServiceDto]].toOption.get)
      _ = assertEquals(
        svcs2.find(_.key == "skea").map(_.tools.map(_.name).sorted),
        Some(List("create_task", "list_tasks"))
      )
      prompt <- json(admin, Method.GET, "/api/admin/prompt").map(_.hcursor.get[String]("text").toOption.get)
      _ = assert(
        prompt.contains("必ず丁寧語で答える") && prompt.contains("skea__create_task") && prompt.contains("起票は SKEA へ"),
        prompt
      )
      // フォームの必須の項目がなければ断る
      _ <- json(
        user,
        Method.POST,
        "/api/sessions",
        Some(StartReq("bug", Map("service" -> "skea")).asJson),
        Status.BadRequest
      )
      // 自分用の前提を登録する（エージェントに渡る）
      _ <- json(user, Method.PUT, "/api/me/context", Some(UserContextDto("担当は経理システムです").asJson))
      // 画像を添付する（PNG 以外の中身は断る）
      png = Array[Byte](0x89.toByte, 'P', 'N', 'G', 13, 10, 26, 10, 0, 0, 0, 13)
      _ <- fixture().app.run(Request[IO](
        Method.POST,
        uri"/api/attachments?name=x.png"
      ).withEntity("not png")(using EntityEncoder.stringEncoder)
        .putHeaders(
          Cookie(RequestCookie("skakapo_session", user)),
          "X-Skakapo" -> "1",
          headers.`Content-Type`(MediaType.image.png)
        ))
        .map(r => assertEquals(r.status, Status.UnsupportedMediaType))
      att <- fixture().app.run(Request[IO](
        Method.POST,
        uri"/api/attachments?name=screen.png"
      ).withEntity(png)(using EntityEncoder.byteArrayEncoder)
        .putHeaders(
          Cookie(RequestCookie("skakapo_session", user)),
          "X-Skakapo" -> "1",
          headers.`Content-Type`(MediaType.image.png)
        ))
        .flatMap(_.as[AttachmentDto])
      // ほかの人の画像は使えない
      _ <- json(
        other,
        Method.POST,
        "/api/sessions",
        Some(StartReq("other", Map("description" -> "x"), List(att.id)).asJson),
        Status.BadRequest
      )
      // 件名を空にすると、仮の題名（内容の最初の行）で始まり、エージェントが set_title で決める
      s <- json(
        user,
        Method.POST,
        "/api/sessions",
        Some(StartReq(
          "bug",
          Map(
            "service" -> "skea",
            "description" -> "期限順に並ばない",
            "urgency" -> "高"
          ),
          List(att.id)
        ).asJson),
        Status.Created
      ).map(_.as[SessionDto].toOption.get)
      _ = assertEquals((s.title, s.titleAuto), ("期限順に並ばない", true))
      // ほかの人には見えない
      _ <- call(other, Method.GET, s"/api/sessions/${s.id}").map(r => assertEquals(r.status, Status.NotFound))
      // ノード（WorkerPool）が実行を取り、返事をするまで待つ
      done <- (json(user, Method.GET, s"/api/sessions/${s.id}/events?after=0&wait=2").map(_.as[EventsDto].toOption.get)
        .flatMap(d =>
          if d.session.status == SessionStatus.Idle && d.events.exists(_.kind == "assistant") &&
            d.run.exists(_.status == "done")
          then IO.pure(Some(d))
          else IO.sleep(300.millis).as(None)
        ))
        .untilDefinedM.timeout(60.seconds)
      kinds = done.events.map(_.kind)
      _ = assert(kinds.contains("tool_call") && kinds.contains("tool_result"), kinds)
      _ = assert(done.events.exists(e =>
        e.kind == "assistant" && e.data.hcursor.get[String]("text").contains("SKEA-7 を起票しました。")
      ))
      _ = assertEquals(done.session.links, List(LinkDto("SKEA-7 の不具合", "https://skea.test/t/SKEA-7")))
      _ = assertEquals(done.run.map(r => (r.turns, r.node)), Some((4, Some("node-a"))))
      // 使用量: この依頼のすべての実行の合計（4ターン. 費用は単価から計算）
      _ = assertEquals(done.usage.map(u => (u.runs, u.turns, u.inputTokens, u.outputTokens)), Some((1, 4, 400L, 80L)))
      _ = assert(done.usage.exists(_.costUsd > 0), done.usage)
      _ = assertEquals((done.session.title, done.session.titleAuto), ("期限の並び順の不具合", false)) // 決まったら、もう自動では変えない
      // 最初の発言に、添付の一覧と画像が入っている（Claude に渡る）. 画像は本人だけが見られる
      tr0 <- new SessionStore(fixture().xa).transcript(s.id)
      _ = assertEquals(tr0.hcursor.downN(0).downField("content").downN(1).get[String]("type"), Right("image"))
      _ <- call(user, Method.GET, s"/api/attachments/${att.id}").map(r => assertEquals(r.status, Status.Ok))
      _ <- call(other, Method.GET, s"/api/attachments/${att.id}").map(r => assertEquals(r.status, Status.NotFound))
      // 利用者が題名を変えたら、もう自動にしない
      _ <- call(user, Method.PATCH, s"/api/sessions/${s.id}", Some(Json.obj("title" -> "期限の並び替え".asJson))).map(r =>
        assertEquals(r.status, Status.NoContent)
      )
      s2 <-
        json(user, Method.GET, s"/api/sessions/${s.id}").map(_.hcursor.downField("session").as[SessionDto].toOption.get)
      _ = assertEquals((s2.title, s2.titleAuto), ("期限の並び替え", false))
      // skea の MCP を、利用者が連携したトークンで呼んだ
      _ = assertEquals(
        calls.asScala.toList.map(c => (c._1, c._2, c._3)),
        List(("create_task", Json.obj("title" -> "並び順".asJson), "skea_user_token"))
      )
      // エージェントに、前提（管理者のコンテキスト・依頼の手順）とツール（skea・リポジトリがないので作業のツールはなし）を渡した
      sys = llm.systems.asScala.head
      _ = assert(sys.contains("必ず丁寧語で答える") && sys.contains("「不具合の修正」の手順") && sys.contains("プロジェクト: SKEA"), sys)
      _ = assert(sys.contains("担当は経理システムです") && sys.contains("set_title"), sys)
      _ = assert(llm.toolNames.asScala.head.contains("set_title"))
      _ = assert(
        llm.toolNames.asScala.head.contains("skea__create_task") && llm.toolNames.asScala.head.contains("record_link"),
        llm.toolNames
      )
      _ = assert(!llm.toolNames.asScala.head.contains("checkout_repository"))
      // 返事をすると、次の実行が入る（会話の続きが transcript に残っている）
      _ <- json(user, Method.POST, s"/api/sessions/${s.id}/messages", Some(MessageReq("ありがとう").asJson), Status.Accepted)
      tr <- new SessionStore(fixture().xa).transcript(s.id)
      _ = assert(
        tr.asArray.exists(a => a.size >= 9 && a.last.hcursor.get[String]("role").contains("user")),
        tr.noSpaces.take(500)
      )
      // 連携をやめると、トークンは消える
      _ <- call(user, Method.DELETE, s"/api/connections/${skea.id}").map(r => assertEquals(r.status, Status.NoContent))
      cs3 <- json(user, Method.GET, "/api/connections").map(_.as[List[ConnectionDto]].toOption.get)
      _ = assert(cs3.find(_.service.key == "skea").exists(!_.connected))
    } yield ()
  }

  test("画面以外（X-Skakapo のない要求）からの更新と、ログインしていない要求は断る") {
    for {
      ck <- login("田中", admin = false)
      r1 <- fixture().app.run(Request[IO](
        Method.POST,
        uri"/api/sessions"
      ).withEntity(StartReq("other", Map("title" -> "a", "description" -> "b")).asJson)
        .putHeaders(Cookie(RequestCookie("skakapo_session", ck))))
      _ = assertEquals(r1.status, Status.Forbidden)
      r2 <- fixture().app.run(Request[IO](Method.GET, uri"/api/sessions"))
      _ = assertEquals(r2.status, Status.Unauthorized)
    } yield ()
  }

  test("クレジット: 実行ごとの費用を積み上げ、上限（既定・人ごと）・全体の予算を超えたら依頼を断る") {
    val start = StartReq("other", Map("description" -> "クレジットの確認"))
    for {
      admin <- login("管理者", admin = true)
      user <- login("鈴木", admin = false)
      me <- json(user, Method.GET, "/api/me").map(_.as[MeDto].toOption.get)
      // 前のテストで使った額がある（管理者の画面の利用者ごとの一覧・全体）
      c0 <- json(admin, Method.GET, "/api/admin/credits")
      _ = assert(c0.hcursor.get[Double]("used").exists(_ > 0), c0.noSpaces)
      // 既定の上限を 0 にすると、依頼を始められない（402）
      st <- json(admin, Method.GET, "/api/admin/settings").map(_.as[AdminSettingsDto].toOption.get)
      _ <- json(
        admin,
        Method.PUT,
        "/api/admin/settings",
        Some(AdminSettingsReq(st.agent.copy(defaultUserCapUsd = Some(0.0)), st.trackers).asJson)
      )
      _ <- json(user, Method.POST, "/api/sessions", Some(start.asJson), Status.PaymentRequired)
      cr <- json(user, Method.GET, "/api/me/credit").map(_.as[CreditDto].toOption.get)
      _ = assertEquals((cr.userCap, cr.userRemaining), (Some(0.0), Some(0.0)))
      // 人ごとに上限なしにすれば始められる
      _ <-
        call(admin, Method.PUT, s"/api/admin/credits/${me.id}", Some(UserCapReq(useDefault = false, None).asJson)).map(
          r => assertEquals(r.status, Status.NoContent)
        )
      _ <- json(user, Method.POST, "/api/sessions", Some(start.asJson), Status.Created)
      // 全体の予算を 0 にすると、だれも始められない
      _ <- json(
        admin,
        Method.PUT,
        "/api/admin/settings",
        Some(AdminSettingsReq(st.agent.copy(monthlyBudgetUsd = Some(0.0)), st.trackers).asJson)
      )
      _ <- json(user, Method.POST, "/api/sessions", Some(start.asJson), Status.PaymentRequired)
      // 元に戻す（既定に戻すと、既定の上限）
      _ <- json(admin, Method.PUT, "/api/admin/settings", Some(AdminSettingsReq(st.agent, st.trackers).asJson))
      _ <- call(admin, Method.PUT, s"/api/admin/credits/${me.id}", Some(UserCapReq(useDefault = true).asJson)).map(r =>
        assertEquals(r.status, Status.NoContent)
      )
      // 管理者以外は、利用者ごとの一覧を見られない
      _ <- call(user, Method.GET, "/api/admin/credits").map(r => assertEquals(r.status, Status.Forbidden))
    } yield ()
  }

  test("サービスのエージェント専用アカウント: キーを確かめて保存し、連携していない利用者の依頼ではそのアカウントで使う") {
    for {
      admin <- login("管理者", admin = true)
      user <- login("連携していない人", admin = false)
      svcs <- json(admin, Method.GET, "/api/admin/services").map(_.as[List[ServiceDto]].toOption.get)
      skea = svcs.find(_.key == "skea").get
      base = ServiceReq(
        skea.key,
        skea.name,
        skea.kind,
        skea.baseUrl,
        skea.mcpUrl,
        skea.description,
        skea.notes,
        skea.oauth,
        None,
        skea.manualToken,
        skea.enabled,
        skea.position,
        agentAccount = "skakapo エージェント"
      )
      // つながらないキーは断る（MCP のツールの一覧を読めない）
      _ <- json(
        admin,
        Method.PUT,
        s"/api/admin/services/${skea.id}",
        Some(base.copy(agentToken = Some("wrong")).asJson),
        Status.BadRequest
      )
      saved <- json(
        admin,
        Method.PUT,
        s"/api/admin/services/${skea.id}",
        Some(base.copy(agentToken = Some("skea_agent_token")).asJson)
      )
      _ = assert(!saved.noSpaces.contains("skea_agent_token"), saved.noSpaces)
      _ =
        assertEquals(saved.as[ServiceDto].map(x => (x.hasAgentToken, x.agentAccount)), Right((true, "skakapo エージェント")))
      // キーを変えずに保存しても消えない
      again <-
        json(admin, Method.PUT, s"/api/admin/services/${skea.id}", Some(base.asJson)).map(_.as[ServiceDto].toOption.get)
      _ = assert(again.hasAgentToken)
      // 連携していない利用者の依頼: skea のツールが渡り、前提に「エージェント専用アカウントで使う」と書かれる
      n0 = llm.systems.size
      s <- json(
        user,
        Method.POST,
        "/api/sessions",
        Some(StartReq("other", Map("description" -> "skea のタスクを調べて")).asJson),
        Status.Created
      )
        .map(_.as[SessionDto].toOption.get)
      _ <- (json(user, Method.GET, s"/api/sessions/${s.id}/events?after=0&wait=2").map(_.as[EventsDto].toOption.get)
        .flatMap(d =>
          if d.run.exists(_.status == "done") then IO.pure(Some(d)) else IO.sleep(300.millis).as(None)
        )).untilDefinedM.timeout(60.seconds)
      sys = llm.systems.asScala.drop(n0).mkString("\n")
      _ = assert(sys.contains("エージェント専用アカウントで使うサービス: skea（アカウント「skakapo エージェント」）"), sys)
      _ = assert(llm.toolNames.asScala.toList.last.contains("skea__create_task"), llm.toolNames.asScala.toList.last)
      // MCP の URL を変えるなら、キーを入れ直さないと保存できない（保存してあるキーを別のホストへ送らせない）
      _ <- json(
        admin,
        Method.PUT,
        s"/api/admin/services/${skea.id}",
        Some(base.copy(mcpUrl = "http://evil.test/mcp").asJson),
        Status.BadRequest
      )
      // 送り先のホストが変わったら、利用者の連携は全員分外れる（利用者のトークンを新しい送り先へ送らない）
      me <- json(user, Method.GET, "/api/me").map(_.as[MeDto].toOption.get)
      _ <-
        sql"""insert into connection (user_id, service_id, token, refresh, scope, via) values (${me.id}, ${skea.id}, 'x', '', '', 'manual')
                 on conflict do nothing""".update.run.transact(fixture().xa)
      before <-
        sql"select count(*) from connection where service_id = ${skea.id}".query[Int].unique.transact(fixture().xa)
      _ = assert(before > 0)
      _ <- json(
        admin,
        Method.PUT,
        s"/api/admin/services/${skea.id}",
        Some(base.copy(baseUrl = "http://skea2.test").asJson)
      )
      after <-
        sql"select count(*) from connection where service_id = ${skea.id}".query[Int].unique.transact(fixture().xa)
      _ = assertEquals(after, 0)
      // キーを消すと、連携していない利用者には skea のツールが渡らない
      _ <- json(admin, Method.PUT, s"/api/admin/services/${skea.id}", Some(base.copy(agentToken = Some("")).asJson))
      cleared <- json(admin, Method.GET, "/api/admin/services").map(_.as[List[ServiceDto]].toOption.get.find(_.key ==
        "skea").get)
      _ = assert(!cleared.hasAgentToken)
    } yield ()
  }

  test("Git の共有アカウント: GitHub App は App ID と読める秘密鍵が要り、保存する前に認証を確かめる") {
    val pem = "-----BEGIN RSA PRIVATE KEY-----\nAAAA\n-----END RSA PRIVATE KEY-----"
    val req = GitAccountReq(
      "app-bot",
      GitProvider.Github,
      "http://skea.test",
      "",
      Some(pem),
      "",
      "",
      GitAuthKind.GithubApp,
      "123456",
      ""
    )
    for {
      admin <- login("管理者", admin = true)
      // App ID がない・Installation ID が数字でない・Forgejo では選べない
      _ <- json(admin, Method.POST, "/api/admin/git-accounts", Some(req.copy(appId = "").asJson), Status.BadRequest)
      _ <- json(
        admin,
        Method.POST,
        "/api/admin/git-accounts",
        Some(req.copy(installationId = "abc").asJson),
        Status.BadRequest
      )
      _ <- json(
        admin,
        Method.POST,
        "/api/admin/git-accounts",
        Some(req.copy(provider = GitProvider.Forgejo, username = "u").asJson),
        Status.BadRequest
      )
      // 読めない秘密鍵・認証できない（GitHub App として /app を読めない）ものは保存しない
      e <- json(admin, Method.POST, "/api/admin/git-accounts", Some(req.asJson), Status.BadRequest)
      _ = assert(e.noSpaces.contains("秘密鍵"), e.noSpaces)
      _ <-
        json(admin, Method.GET, "/api/admin/git-accounts").map(j => assert(!j.noSpaces.contains("app-bot"), j.noSpaces))
    } yield ()
  }

  test("利用者の種類: IdP のグループか管理者が付けた種類で決まり、その回答の決まりが前提に入る") {
    def devLogin(name: String, groups: List[String]): IO[String] =
      fixture().app.run(Request[IO](
        Method.POST,
        uri"/auth/dev-login"
      ).withEntity(Json.obj("name" -> name.asJson, "groups" -> groups.asJson))
        .putHeaders("X-Skakapo" -> "1")).map(r =>
        r.cookies.find(_.name == "skakapo_session").map(_.content).getOrElse(fail("no cookie"))
      )
    for {
      admin <- login("管理者", admin = true)
      user <- devLogin("運用の人", List("ops-team"))
      _ <- call(user, Method.GET, "/api/admin/user-groups").map(r => assertEquals(r.status, Status.Forbidden))
      // 最初から engineer と operator がある
      gs <- json(admin, Method.GET, "/api/admin/user-groups").map(_.as[List[UserGroupDto]].toOption.get)
      _ = assertEquals(gs.map(_.key).sorted, List("engineer", "operator"))
      ops = gs.find(_.key == "operator").get
      eng = gs.find(_.key == "engineer").get
      // どれにも当てはまらない人はオペレータ（初期データで既定）
      _ = assertEquals((ops.label, ops.isDefault, eng.isDefault), ("オペレータ", true, false))
      // operator に IdP のグループ ops-team を対応させると、その人は operator になる
      _ <- json(
        admin,
        Method.PUT,
        s"/api/admin/user-groups/${ops.id}",
        Some(UserGroupReq(
          ops.key,
          ops.label,
          ops.description,
          ops.instructions,
          List("ops-team"),
          false,
          ops.position
        ).asJson)
      )
      us <- json(admin, Method.GET, "/api/admin/users").map(_.as[List[AdminUserDto]].toOption.get)
      me = us.find(_.user.name == "運用の人").get
      _ = assertEquals((me.idpGroups, me.effective), (List("ops-team"), Some(ops.label)))
      // 依頼すると、operator の回答の決まりが前提に入る
      n0 = llm.systems.size
      s <- json(
        user,
        Method.POST,
        "/api/sessions",
        Some(StartReq("other", Map("description" -> "注文が見られない")).asJson),
        Status.Created
      )
        .map(_.as[SessionDto].toOption.get)
      _ <- (json(user, Method.GET, s"/api/sessions/${s.id}/events?after=0&wait=2").map(_.as[EventsDto].toOption.get)
        .flatMap(d =>
          if d.run.exists(_.status == "done") then IO.pure(Some(d)) else IO.sleep(300.millis).as(None)
        )).untilDefinedM.timeout(60.seconds)
      _ = assert(llm.systems.asScala.drop(n0).exists(_.contains(s"# 利用者の種類: ${ops.label}")))
      // 管理者が engineer を付けると、IdP のグループより優先する. 外すと IdP のグループに戻る
      _ <-
        call(admin, Method.PUT, s"/api/admin/users/${me.user.id}/group", Some(AssignGroupReq(Some(eng.id)).asJson)).map(
          r => assertEquals(r.status, Status.NoContent)
        )
      e1 <- json(admin, Method.GET, "/api/admin/users").map(_.as[List[AdminUserDto]].toOption.get.find(_.user.id ==
        me.user.id).get)
      _ = assertEquals(e1.effective, Some(eng.label))
      _ <- call(admin, Method.PUT, s"/api/admin/users/${me.user.id}/group", Some(AssignGroupReq(None).asJson))
      e2 <- json(admin, Method.GET, "/api/admin/users").map(_.as[List[AdminUserDto]].toOption.get.find(_.user.id ==
        me.user.id).get)
      _ = assertEquals(e2.effective, Some(ops.label))
      // キーが正しくない種類は保存しない
      _ <-
        json(admin, Method.POST, "/api/admin/user-groups", Some(UserGroupReq("Bad Key", "x").asJson), Status.BadRequest)
    } yield ()
  }

  test("細かい進捗の表示: 既定は出さず、利用者ごとに設定できる") {
    for {
      user <- login("進捗を見たい人", admin = false)
      m0 <- json(user, Method.GET, "/api/me").map(_.as[MeDto].toOption.get)
      _ = assert(!m0.showDetails)
      m1 <- json(
        user,
        Method.PATCH,
        "/api/me",
        Some(UpdateMeReq(None, None, Some(true)).asJson)
      ).map(_.as[MeDto].toOption.get)
      _ = assert(m1.showDetails)
      // 名前だけを変えても、設定は変わらない
      m2 <- json(
        user,
        Method.PATCH,
        "/api/me",
        Some(UpdateMeReq(Some("進捗を見たい人2"), None).asJson)
      ).map(_.as[MeDto].toOption.get)
      _ = assertEquals((m2.name, m2.showDetails), ("進捗を見たい人2", true))
    } yield ()
  }

  test("クラウドの資格情報: 項目を確かめ、認証できないものは保存しない. 管理者だけ") {
    val aws = CloudCredentialReq(
      "aws-dev",
      CloudProvider.Aws,
      Map("accessKeyId" -> "AKIDEXAMPLE", "region" -> "ap-northeast-1"),
      Some(Map("secretAccessKey" -> "secret")),
      "開発用"
    )
    for {
      admin <- login("管理者", admin = true)
      user <- login("クラウドを見たい人", admin = false)
      _ <- call(user, Method.GET, "/api/admin/cloud-credentials").map(r => assertEquals(r.status, Status.Forbidden))
      // 必須の項目がない・秘密がない・GCP の鍵が JSON でない
      _ <- json(
        admin,
        Method.POST,
        "/api/admin/cloud-credentials",
        Some(aws.copy(config = Map("region" -> "ap-northeast-1")).asJson),
        Status.BadRequest
      )
      _ <- json(
        admin,
        Method.POST,
        "/api/admin/cloud-credentials",
        Some(aws.copy(secrets = None).asJson),
        Status.BadRequest
      )
      _ <- json(
        admin,
        Method.POST,
        "/api/admin/cloud-credentials",
        Some(CloudCredentialReq(
          "gcp",
          CloudProvider.Gcp,
          Map.empty,
          Some(Map("serviceAccountJson" -> "not json"))
        ).asJson),
        Status.BadRequest
      )
      // 認証できない（STS に届かない）ものは保存しない
      e <- json(admin, Method.POST, "/api/admin/cloud-credentials", Some(aws.asJson), Status.BadRequest)
      _ = assert(e.noSpaces.contains("AWS で認証できませんでした"), e.noSpaces)
      _ <-
        json(admin, Method.GET, "/api/admin/cloud-credentials").map(j => assertEquals(j.asArray.map(_.size), Some(0)))
    } yield ()
  }

  test("Git の共有アカウント: Forgejo はユーザー名が必須") {
    for {
      admin <- login("管理者", admin = true)
      req = GitAccountReq("forgejo-bot", GitProvider.Forgejo, "", "", Some("tok"), "skakapo", "bot@example.com")
      _ <- json(admin, Method.POST, "/api/admin/git-accounts", Some(req.asJson), Status.BadRequest)
      a <- json(
        admin,
        Method.POST,
        "/api/admin/git-accounts",
        Some(req.copy(username = "skakapo-bot").asJson),
        Status.Created
      )
      _ <- call(admin, Method.DELETE, s"/api/admin/git-accounts/${a.hcursor.get[String]("id").toOption.get}")
    } yield ()
  }

  test("Git の共有アカウント: トークンは暗号化して保存し、画面に返さない. リポジトリに結び付けられる") {
    val testBox = SecretBox("test-app-secret-0123456789")
    val req = GitAccountReq("bot", GitProvider.Github, "", "", Some("ghp_secret_value"), "skakapo", "bot@example.com")
    for {
      admin <- login("管理者", admin = true)
      user <- login("木村", admin = false)
      // トークンなしでは作れない. user:pass@ 入りの URL は断る
      _ <- json(admin, Method.POST, "/api/admin/git-accounts", Some(req.copy(token = None).asJson), Status.BadRequest)
      _ <- json(
        admin,
        Method.POST,
        "/api/admin/git-accounts",
        Some(req.copy(apiUrl = "https://u:p@api.github.com").asJson),
        Status.BadRequest
      )
      a <- json(admin, Method.POST, "/api/admin/git-accounts", Some(req.asJson), Status.Created)
      _ = assert(!a.noSpaces.contains("ghp_secret_value"), a.noSpaces)
      acc = a.as[GitAccountDto].toOption.get
      _ = assert(acc.hasToken)
      // DB にも平文では残らない. 復号すると元に戻る
      enc <- sql"select token from git_account where id = ${acc.id}".query[String].unique.transact(fixture().xa)
      _ = assert(!enc.contains("ghp_secret_value"))
      _ = assertEquals(testBox.decrypt(enc), Some("ghp_secret_value"))
      // API の URL を変えるなら、秘密を入れ直さないと保存できない
      _ <- json(
        admin,
        Method.PUT,
        s"/api/admin/git-accounts/${acc.id}",
        Some(req.copy(token = None, apiUrl = "https://evil.example.com").asJson),
        Status.BadRequest
      )
      // トークンを空で更新すると、前のトークンのまま
      _ <- json(
        admin,
        Method.PUT,
        s"/api/admin/git-accounts/${acc.id}",
        Some(req.copy(token = None, authorName = "skakapo-bot").asJson)
      )
      t <- new ConfigStore(fixture().xa).gitAccountWithToken(acc.id)
      _ = assertEquals(
        t.map(x => (x._1.authorName, testBox.decrypt(x._2))),
        Some(("skakapo-bot", Some("ghp_secret_value")))
      )
      // リポジトリに結び付ける（存在しないアカウント・user:pass@ 入りの URL は断る）
      repo = RepositoryReq("shared-repo", "https://github.com/example/shared-repo", accountId = Some(acc.id))
      // 共有アカウント（github.com）と違うホストのリポジトリには結び付けない
      _ <- json(
        admin,
        Method.POST,
        "/api/admin/repositories",
        Some(repo.copy(url = "https://evil.example.com/example/r").asJson),
        Status.BadRequest
      )
      _ <- json(
        admin,
        Method.POST,
        "/api/admin/repositories",
        Some(repo.copy(accountId = Some(UUID.randomUUID())).asJson),
        Status.BadRequest
      )
      _ <- json(
        admin,
        Method.POST,
        "/api/admin/repositories",
        Some(repo.copy(url = "https://x:tok@github.com/example/r").asJson),
        Status.BadRequest
      )
      r <- json(
        admin,
        Method.POST,
        "/api/admin/repositories",
        Some(repo.asJson),
        Status.Created
      ).map(_.as[RepositoryDto].toOption.get)
      _ = assertEquals(r.accountId, Some(acc.id))
      // 管理者以外は触れない
      _ <- call(user, Method.GET, "/api/admin/git-accounts").map(x => assertEquals(x.status, Status.Forbidden))
      // アカウントを消すと、リポジトリは利用者の連携に戻る
      _ <- call(admin, Method.DELETE, s"/api/admin/git-accounts/${acc.id}").map(x =>
        assertEquals(x.status, Status.NoContent)
      )
      rs <- json(admin, Method.GET, "/api/admin/repositories").map(_.as[List[RepositoryDto]].toOption.get)
      _ = assertEquals(rs.find(_.id == r.id).flatMap(_.accountId), None)
      _ <- call(admin, Method.DELETE, s"/api/admin/repositories/${r.id}")
    } yield ()
  }

  private def upload(ck: String, name: String, ct: MediaType, data: Array[Byte]): IO[Response[IO]] =
    fixture().app.run(Request[IO](
      Method.POST,
      Uri.unsafeFromString(s"/api/attachments?name=${java.net.URLEncoder.encode(name, "UTF-8")}")
    )
      .withEntity(data)(using EntityEncoder.byteArrayEncoder)
      .putHeaders(Cookie(RequestCookie("skakapo_session", ck)), "X-Skakapo" -> "1", headers.`Content-Type`(ct)))

  private def idle(ck: String, id: UUID): IO[EventsDto] =
    (json(ck, Method.GET, s"/api/sessions/$id/events?after=0&wait=2").map(_.as[EventsDto].toOption.get)
      .flatMap(d =>
        if d.session.status == SessionStatus.Idle && d.run.forall(_.status == "done") then IO.pure(Some(d))
        else IO.sleep(300.millis).as(None)
      ))
      .untilDefinedM.timeout(60.seconds)

  test("添付: 種類を問わず上げられ、画像・PDF・文字のファイルは Claude が読める形で、それ以外は名前だけを渡す. 返事にも添付できる") {
    val utf8 = java.nio.charset.StandardCharsets.UTF_8
    for {
      user <- login("添付する人", admin = false)
      csv <- upload(user, "売上.csv", MediaType.text.csv, "月,額\n1月,100\n".getBytes(utf8)).flatMap(_.as[AttachmentDto])
      pdf <-
        upload(user, "仕様.pdf", MediaType.application.pdf, "%PDF-1.4\n%%EOF".getBytes(utf8)).flatMap(_.as[AttachmentDto])
      bin <- upload(
        user,
        "dump.bin",
        MediaType.application.`octet-stream`,
        Array[Byte](0, 1, 2, -1, -2)
      ).flatMap(_.as[AttachmentDto])
      _ = assertEquals(
        List(csv.contentType, pdf.contentType, bin.contentType),
        List("text/csv", "application/pdf", "application/octet-stream")
      )
      // 大きすぎるものは断る
      _ <- upload(user, "big.bin", MediaType.application.`octet-stream`, new Array[Byte](AttachmentRule.MaxBytes + 1))
        .map(r => assertEquals(r.status, Status.PayloadTooLarge))
      s <- json(
        user,
        Method.POST,
        "/api/sessions",
        Some(StartReq("other", Map("description" -> "資料を見てください"), List(csv.id, pdf.id, bin.id)).asJson),
        Status.Created
      ).map(_.as[SessionDto].toOption.get)
      tr <- new SessionStore(fixture().xa).transcript(s.id)
      content = tr.hcursor.downN(0).downField("content")
      types = (0 to 4).toList.map(i => content.downN(i).get[String]("type").getOrElse(""))
      // 一覧（文）・CSV（文書）・PDF（文書）・フォームの文. バイナリは一覧に名前だけ
      _ = assertEquals(types, List("text", "document", "document", "text", ""))
      _ = assert(
        content.downN(0).get[String]("text").exists(t => t.contains("dump.bin") && t.contains("中身を読めません")),
        content.focus
      )
      _ = assertEquals(content.downN(1).downField("source").get[String]("data"), Right("月,額\n1月,100\n"))
      _ = assertEquals(content.downN(2).downField("source").get[String]("media_type"), Right("application/pdf"))
      // 画像でないものは、ダウンロードさせる
      _ <- call(user, Method.GET, s"/api/attachments/${csv.id}").map { r =>
        assertEquals(r.contentType.map(_.mediaType), Some(MediaType.application.`octet-stream`))
        assert(
          r.headers.get(
            org.typelevel.ci.CIString("Content-Disposition")
          ).exists(_.head.value.startsWith("attachment;")),
          r.headers
        )
      }
      _ <- idle(user, s.id)
      // 返事にも添付できる（文がなくてもよい）. 出来事に添付が残る
      log <- upload(user, "error.log", MediaType.text.plain, "ERROR x".getBytes(utf8)).flatMap(_.as[AttachmentDto])
      _ <- json(
        user,
        Method.POST,
        s"/api/sessions/${s.id}/messages",
        Some(MessageReq("", List(log.id)).asJson),
        Status.Accepted
      )
      tr2 <- new SessionStore(fixture().xa).transcript(s.id)
      last = tr2.asArray.get.last.hcursor.downField("content")
      _ = assertEquals(
        (0 to 2).toList.map(i => last.downN(i).get[String]("type").getOrElse("")),
        List("text", "document", "text")
      )
      ev <- json(user, Method.GET, s"/api/sessions/${s.id}/events?after=0").map(_.as[EventsDto].toOption.get)
      _ = assert(ev.events.exists(e =>
        e.kind == "user" && e.data.hcursor.get[List[AttachmentDto]]("attachments").exists(_.map(_.id) == List(log.id))
      ))
      // 一度使った添付は、もう使えない
      _ <- idle(user, s.id)
      _ <- json(
        user,
        Method.POST,
        s"/api/sessions/${s.id}/messages",
        Some(MessageReq("もう一度", List(log.id)).asJson),
        Status.BadRequest
      )
    } yield ()
  }

  test("費用の計算（モデルの単価. キャッシュの読み込みは安く、書き込みは入力の2倍）") {
    assertEqualsDouble(Models.cost("claude-opus-5-5", 1000000, 0, 0, 0), 4.0, 1e-9)
    assertEqualsDouble(Models.cost("claude-opus-5-5", 0, 1000000, 0, 0), 20.0, 1e-9)
    assertEqualsDouble(Models.cost("claude-opus-5-5", 0, 0, 1000000, 1000000), 0.2 + 8.0, 1e-9)
    assertEqualsDouble(Models.cost("claude-haiku-5-5", 1000000, 1000000, 0, 0), 0.6, 1e-9)
    assertEquals(Usd.fmt(1.006), "$1.01")
    assertEquals(Usd.fmt(12.3), "$12.30")
  }
}

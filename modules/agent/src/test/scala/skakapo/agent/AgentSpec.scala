package skakapo.agent

import cats.effect.IO
import com.anthropic.core.JsonValue
import com.anthropic.models.messages.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import munit.CatsEffectSuite
import skakapo.shared.RepositoryDto

import java.nio.file.{Files, Path}
import java.util.UUID
import scala.jdk.CollectionConverters.*

class AgentSpec extends CatsEffectSuite {

  test("会話（MessageParam の配列）を JSON にして読み戻せる（別のノードが続きから読めるように）") {
    val ms = List(
      MessageParam.ofUser("こんにちは"),
      MessageParam.ofAssistantOfContentBlockParams(List(
        ContentBlockParam.ofText(TextBlockParam.builder().text("調べます").build()),
        ContentBlockParam.ofToolUse(ToolUseBlockParam.builder().id("toolu_1").name("skea__list_tasks")
          .input(ToolUseBlockParam.Input.builder().putAdditionalProperty(
            "project",
            JsonValue.from("SKEA")
          ).build()).build())
      ).asJava),
      MessageParam.ofUserOfContentBlockParams(List(ContentBlockParam.ofToolResult(
        ToolResultBlockParam.builder().toolUseId("toolu_1").content("SKEA-1 タスク").isError(false).build()
      )).asJava)
    )
    val j = Jsons.encodeTranscript(ms)
    assertEquals(j.asArray.map(_.size), Some(3))
    assertEquals(j.hcursor.downN(1).downField("content").downN(1).get[String]("name"), Right("skea__list_tasks"))
    val back = Jsons.decodeTranscript(j)
    assertEquals(Jsons.encodeTranscript(back), j)
  }

  test("MCP の inputSchema を Claude のツールのスキーマにする") {
    val s = parse(
      """{"type":"object","properties":{"q":{"type":"string","description":"検索"}},"required":["q"]}"""
    ).toOption.get
    val schema = Jsons.inputSchema(s)
    val t = Tool.builder().name("x").inputSchema(schema).build()
    val j = parse(com.anthropic.core.ObjectMappers.jsonMapper().writeValueAsString(t)).toOption.get
    assertEquals(
      j.hcursor.downField("input_schema").downField("properties").downField("q").get[String]("type"),
      Right("string")
    )
    assertEquals(j.hcursor.downField("input_schema").get[List[String]]("required"), Right(List("q")))
  }

  private def tmp(): Path = Files.createTempDirectory("skakapo-test")

  test("作業場所の外・.git の中は扱わない") {
    val ws = Workspace.forSession(tmp(), UUID.randomUUID())
    assert(ws.resolve("/workspace/a/b.txt").isRight)
    assert(ws.resolve("a/../b").isRight)
    assert(ws.resolve("/workspace/../../etc/passwd").isLeft)
    assert(ws.resolve("../x").isLeft)
    assert(ws.resolve("/workspace/repo/.git/config").isLeft)
    assertEquals(ws.resolve("/workspace").map(ws.show), Right("/workspace"))
  }

  test("text editor（view・create・str_replace・insert）と search_code") {
    val ws = Workspace.forSession(tmp(), UUID.randomUUID())
    def ed(j: Json) = ws.textEditor(j.hcursor)
    def create(path: String) =
      ed(Json.obj("command" -> "create".asJson, "path" -> path.asJson, "file_text" -> "x".asJson))
    for {
      // 取り出していない場所・/workspace の直下（偽の git の管理ファイル）には書けない
      n1 <- create("/workspace/r/a.txt")
      n2 <- create("/workspace/config")
      _ = assert(n1.isError && n2.isError, n1.text + n2.text)
      _ <- IO.blocking(Files.createDirectories(ws.repoDir("r").resolve(".git")))
      c <- ed(Json.obj(
        "command" -> "create".asJson,
        "path" -> "/workspace/r/a.txt".asJson,
        "file_text" -> "one\ntwo\nthree".asJson
      ))
      _ = assert(!c.isError, c.text)
      v <- ed(Json.obj(
        "command" -> "view".asJson,
        "path" -> "/workspace/r/a.txt".asJson,
        "view_range" -> List(2, 3).asJson
      ))
      _ = assert(v.text.contains("2\ttwo") && v.text.contains("3\tthree") && !v.text.contains("one"), v.text)
      r <- ed(Json.obj(
        "command" -> "str_replace".asJson,
        "path" -> "r/a.txt".asJson,
        "old_str" -> "two".asJson,
        "new_str" -> "2".asJson
      ))
      _ = assert(!r.isError, r.text)
      n <- ed(Json.obj(
        "command" -> "str_replace".asJson,
        "path" -> "r/a.txt".asJson,
        "old_str" -> "nothing".asJson,
        "new_str" -> "x".asJson
      ))
      _ = assert(n.isError)
      i <- ed(Json.obj(
        "command" -> "insert".asJson,
        "path" -> "r/a.txt".asJson,
        "insert_line" -> 1.asJson,
        "insert_text" -> "1.5".asJson
      ))
      _ = assert(!i.isError, i.text)
      _ = assertEquals(Files.readString(ws.root.resolve("r/a.txt")), "one\n1.5\n2\nthree")
      s <- ws.search("thr+ee", "/workspace", None)
      _ = assert(s.text.contains("/workspace/r/a.txt:4: three"), s.text)
      out <- ed(Json.obj(
        "command" -> "create".asJson,
        "path" -> "/workspace/../escape.txt".asJson,
        "file_text" -> "x".asJson
      ))
      _ = assert(out.isError)
    } yield ()
  }

  test("リポジトリを取り出し、skakapo/ のブランチにだけ commit と push をする") {
    val base = tmp()
    val origin = base.resolve("origin.git")
    val seed = base.resolve("seed")
    val ws = Workspace.forSession(base, UUID.randomUUID())
    val repo = RepositoryDto(UUID.randomUUID(), "demo", origin.toUri.toString, "main", "", "", "", true)
    def g(dir: Path, args: String*) =
      ws.git(dir, None, scala.concurrent.duration.Duration(30, "s"), args*).map(r => assertEquals(r._1, 0, r._2))
    for {
      _ <- IO.blocking(Files.createDirectories(seed))
      _ <- g(base, "init", "--bare", "-b", "main", origin.toString)
      _ <- g(seed, "init", "-b", "main")
      _ <- IO.blocking(Files.writeString(seed.resolve("README.md"), "hello\n"))
      _ <- g(seed, "add", "-A")
      _ <- g(seed, "-c", "user.name=t", "-c", "user.email=t@example.com", "commit", "-m", "init")
      _ <- g(seed, "push", origin.toString, "main")
      co <- ws.checkout(repo, None, Some("skakapo/test-1"))
      _ = assert(!co.isError, co.text)
      _ = assert(co.text.contains("skakapo/test-1"), co.text)
      _ <- IO.blocking(Files.writeString(ws.repoDir("demo").resolve("README.md"), "hello world\n"))
      d <- ws.diff(repo)
      _ = assert(d.text.contains("+hello world"), d.text)
      bad <- ws.commitAndPush(repo, "main", "x", None, ("t", "t@example.com"))
      _ = assert(bad.isError)
      ok <- ws.commitAndPush(repo, "skakapo/test-1", "README を直す", None, ("t", "t@example.com"))
      _ = assert(!ok.isError, ok.text)
      ls <- ws.git(
        base,
        None,
        scala.concurrent.duration.Duration(30, "s"),
        "--git-dir",
        origin.toString,
        "branch",
        "--list"
      )
      _ = assert(ls._2.contains("skakapo/test-1"), ls._2)
      // CI の定義を変えるコミットは断る
      _ <- IO.blocking {
        val w = ws.repoDir("demo").resolve(".github/workflows"); Files.createDirectories(w);
        Files.writeString(w.resolve("ü あ.yml"), "on: push\n")
      }
      ci <- ws.commitAndPush(repo, "skakapo/test-1", "CI を足す", None, ("t", "t@example.com"))
      _ = assert(ci.isError && ci.text.contains("CI の定義"), ci.text)
      // 作業場所の直下に偽の管理ファイルがあっても、取り出したリポジトリの .git だけを使う
      _ <- IO.blocking {
        Files.writeString(ws.root.resolve("HEAD"), "ref: refs/heads/evil\n");
        Files.writeString(ws.root.resolve("config"), "[core]\n\tbare = false\n")
      }
      d2 <- ws.diff(repo)
      _ = assert(!d2.isError && !d2.text.contains("evil"), d2.text)
    } yield ()
  }

  test("Git の認証のヘッダーは、リポジトリのホストにだけ付ける") {
    assertEquals(GitAuth.origin("https://github.com/o/r.git"), Some("https://github.com"))
    assertEquals(GitAuth.origin("https://ghe.example.com:8443/o/r"), Some("https://ghe.example.com:8443"))
    assertEquals(GitAuth.origin("https://u:p@github.com/o/r"), None)
    assertEquals(GitAuth.origin("file:///tmp/x"), None)
  }

  test("GitHub の URL からオーナーとリポジトリ・API の場所") {
    val gh =
      new GitHosts(org.http4s.client.Client[IO](_ => cats.effect.Resource.eval(IO.raiseError(new Exception("no")))))
    assertEquals(gh.ownerRepo("https://github.com/Ondeoma/skea.git"), Some("Ondeoma" -> "skea"))
    assertEquals(gh.ownerRepo("https://github.com/Ondeoma/skea"), Some("Ondeoma" -> "skea"))
    assertEquals(gh.ownerRepo("https://github.com/Ondeoma"), None)
    assertEquals(gh.apiBase("https://github.com"), "https://api.github.com")
    assertEquals(gh.apiBase("https://ghe.example.com/"), "https://ghe.example.com/api/v3")
  }

  test("会話の修復: 結果のない tool_use に「分からない」を補う（途中・末尾・文字の発言のどれでも）") {
    def use(id: String) =
      Json.obj("type" -> "tool_use".asJson, "id" -> id.asJson, "name" -> "x".asJson, "input" -> Json.obj())
    def res(id: String) = Json.obj("type" -> "tool_result".asJson, "tool_use_id" -> id.asJson, "content" -> "ok".asJson)
    def msg(role: String, content: Json) = Json.obj("role" -> role.asJson, "content" -> content)
    val t = Json.arr(
      msg("user", "はじめ".asJson),
      // a1 だけ結果がある（a2 は欠けている）
      msg("assistant", Json.arr(use("a1"), use("a2"))),
      msg("user", Json.arr(res("a1"), Json.obj("type" -> "text".asJson, "text" -> "続けて".asJson))),
      // 途中で止まったあとに、利用者が文字だけで返事をした
      msg("assistant", Json.arr(use("b1"))),
      msg("user", "まだですか".asJson),
      // 末尾で切れた
      msg("assistant", Json.arr(use("c1")))
    )
    val r = Transcript.repair(t).asArray.get.toList
    def types(m: Json) = m.hcursor.downField("content").as[List[Json]].getOrElse(Nil).map(b =>
      b.hcursor.get[String]("type").getOrElse("") + b.hcursor.get[String]("tool_use_id").fold(_ => "", ":" + _)
    )
    assertEquals(r.size, 7)
    // 結果は利用者の発言の先頭に並ぶ（欠けていたものが先、元の結果、文字の順）
    assertEquals(types(r(2)), List("tool_result:a2", "tool_result:a1", "text"))
    assertEquals(types(r(4)), List("tool_result:b1", "text"))
    assertEquals(r(4).hcursor.downField("content").downN(1).get[String]("text"), Right("まだですか"))
    assertEquals(types(r(6)), List("tool_result:c1"))
    assertEquals(r(6).hcursor.downField("content").downN(0).get[Boolean]("is_error"), Right(true))
    // 直すところがなければ、そのまま
    assertEquals(Transcript.repair(Json.arr(t.asArray.get.take(1)*)), Json.arr(t.asArray.get.take(1)*))
  }

  test("search_code: 重い正規表現でも期限で止まる（Deadline）") {
    val line = "a" * 200
    val until = System.nanoTime() + 200_000_000L
    val t0 = System.nanoTime()
    intercept[Workspace.TimedOut](java.util.regex.Pattern.compile("(.*a){210}").matcher(Workspace.Deadline(
      line,
      until
    )).find())
    assert(System.nanoTime() - t0 < 5_000_000_000L)
  }

  test("Forgejo: API の場所はリポジトリのホストの /api/v1. プルリクエストは GitHub と同じ形で、token の方式で認証する") {
    import org.http4s.*
    assertEquals(
      GitHosts.apiFor(skakapo.shared.GitProvider.Forgejo, "", "https://code.example.com/team/app.git"),
      "https://code.example.com/api/v1"
    )
    assertEquals(
      GitHosts.apiFor(
        skakapo.shared.GitProvider.Forgejo,
        "https://git.example.com/api/v1/",
        "https://code.example.com/team/app"
      ),
      "https://git.example.com/api/v1"
    )
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[(String, String, String)]()
    val http = org.http4s.client.Client[IO] { req =>
      cats.effect.Resource.eval(req.bodyText.compile.string.map { b =>
        seen.add((
          s"${req.method} ${req.uri}",
          req.headers.get(org.typelevel.ci.CIString("Authorization")).map(_.head.value).getOrElse(""),
          b
        ))
        Response[IO](
          Status.Created
        ).withEntity("""{"html_url":"https://code.example.com/team/app/pulls/3","number":3}""")
      })
    }
    val repo = RepositoryDto(UUID.randomUUID(), "app", "https://code.example.com/team/app", "main", "", "", "", true)
    val c = RepoCreds(
      skakapo.shared.GitProvider.Forgejo,
      "https://code.example.com/api/v1",
      GitAuth("bot", "tok"),
      None,
      shared = true,
      "bot"
    )
    new GitHosts(http).createPullRequest(c, repo, "直す", "本文", "skakapo/x", "main", draft = true).map { o =>
      assert(!o.isError, o.text)
      val (line, auth, body) = seen.peek()
      assertEquals(line, "POST https://code.example.com/api/v1/repos/team/app/pulls")
      assertEquals(auth, "token tok")
      assert(body.contains("\"WIP: 直す\"") && !body.contains("draft"), body)
    }
  }

  private def rsaPem(pkcs1: Boolean): (String, java.security.PublicKey) = {
    val g = java.security.KeyPairGenerator.getInstance("RSA"); g.initialize(2048)
    val kp = g.generateKeyPair()
    val der = kp.getPrivate.getEncoded
    // PKCS#8 の頭（2048 ビットの鍵では 26 バイト）を外すと PKCS#1
    val (label, body) = if pkcs1 then ("RSA PRIVATE KEY", der.drop(26)) else ("PRIVATE KEY", der)
    (
      s"-----BEGIN $label-----\n${java.util.Base64.getMimeEncoder(64, "\n".getBytes).encodeToString(body)}\n-----END $label-----\n",
      kp.getPublic
    )
  }

  test("GitHub App の JWT: PKCS#1・PKCS#8 の秘密鍵で RS256 に署名する（iss・期限）") {
    List(true, false).foreach { p1 =>
      val (pem, pub) = rsaPem(p1)
      val key = AppJwt.privateKey(pem).fold(e => fail(e), identity)
      val jwt = AppJwt.sign("123456", key, 1_700_000_000L)
      val Array(h, b, sig) = jwt.split('.')
      val v = java.security.Signature.getInstance("SHA256withRSA"); v.initVerify(pub); v.update(s"$h.$b".getBytes)
      assert(v.verify(java.util.Base64.getUrlDecoder.decode(sig)), s"pkcs1=$p1")
      val claims = parse(new String(java.util.Base64.getUrlDecoder.decode(b))).toOption.get.hcursor
      assertEquals(
        (claims.get[String]("iss"), claims.get[Long]("iat"), claims.get[Long]("exp")),
        (Right("123456"), Right(1_699_999_940L), Right(1_700_000_540L))
      )
    }
    assert(AppJwt.privateKey("not a key").isLeft)
  }

  test("GitHub App: インストールを探し、そのリポジトリだけのトークンを発行して使い回す. 作者は App のボット") {
    import org.http4s.*
    import org.http4s.dsl.io.*
    val (pem, _) = rsaPem(true)
    val issued = new java.util.concurrent.atomic.AtomicInteger(0)
    val bodies = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val app = HttpRoutes.of[IO] {
      case GET -> Root / "api" / "v3" / "repos" / "team" / "app" / "installation" => Ok("""{"id":42}""")
      case r @ POST -> Root / "api" / "v3" / "app" / "installations" / "42" / "access_tokens" =>
        r.bodyText.compile.string.flatMap { b =>
          IO(bodies.add(b)) *> IO(issued.incrementAndGet()) *>
            Ok("""{"token":"ghs_x","expires_at":"2999-01-01T00:00:00Z"}""")
        }
      case GET -> Root / "api" / "v3" / "app" => Ok("""{"slug":"skakapo-bot"}""")
      case GET -> Root / "api" / "v3" / "users" / "skakapo-bot[bot]" => Ok("""{"id":99}""")
    }
    val http = org.http4s.client.Client.fromHttpApp(app.orNotFound)
    val hosts = new GitHosts(http)
    val acc = skakapo.shared.GitAccountDto(
      UUID.randomUUID(),
      "bot",
      skakapo.shared.GitProvider.Github,
      "",
      "",
      true,
      "",
      "",
      skakapo.shared.GitAuthKind.GithubApp,
      "123456",
      ""
    )
    val repo = RepositoryDto(UUID.randomUUID(), "app", "https://ghe.example.com/team/app", "main", "", "", "", true)
    for {
      t1 <- hosts.apps.token(acc, pem, repo, "http://ghe.test/api/v3")
      t2 <- hosts.apps.token(acc, pem, repo, "http://ghe.test/api/v3")
      p <- hosts.apps.probe(acc, pem, "http://ghe.test/api/v3")
    } yield {
      val t = t1.fold(e => fail(e), identity)
      assertEquals(t.auth, GitAuth("x-access-token", "ghs_x"))
      assertEquals(t.author, Some("skakapo-bot[bot]" -> "99+skakapo-bot[bot]@users.noreply.github.com"))
      assertEquals(t2.map(_.auth), Right(t.auth))
      assertEquals(issued.get(), 1)
      val b = parse(bodies.peek()).toOption.get.hcursor
      assertEquals(b.get[List[String]]("repositories"), Right(List("app")))
      assertEquals(b.downField("permissions").get[String]("contents"), Right("write"))
      assertEquals(p, Right("skakapo-bot"))
    }
  }

  test("Bitbucket の OAuth コンシューマー: client credentials でトークンを発行する（git は x-token-auth）") {
    import org.http4s.*
    import org.http4s.dsl.io.*
    import org.http4s.headers.Authorization
    val routes = HttpRoutes.of[IO] {
      case r @ POST -> Root / "site" / "oauth2" / "access_token" =>
        r.headers.get[Authorization].map(_.credentials) match {
          case Some(BasicCredentials("key", "secret")) => Ok("""{"access_token":"bb_x","expires_in":7200}""")
          case _ => Forbidden("""{"error_description":"bad"}""")
        }
    }
    val http = org.http4s.client.Client.fromHttpApp(routes.orNotFound)
    val hosts = new GitHosts(http)
    val acc = skakapo.shared.GitAccountDto(
      UUID.randomUUID(),
      "bb",
      skakapo.shared.GitProvider.Bitbucket,
      "",
      "",
      true,
      "",
      "",
      skakapo.shared.GitAuthKind.BitbucketOAuth,
      "key",
      ""
    )
    val repo = RepositoryDto(UUID.randomUUID(), "r", "https://bitbucket.org/ws/r", "main", "", "", "", true)
    for {
      ok <- hosts.apps.token(acc, "secret", repo, "https://api.bitbucket.org/2.0")
      bad <- hosts.apps.token(acc.copy(id = UUID.randomUUID()), "wrong", repo, "https://api.bitbucket.org/2.0")
    } yield {
      assertEquals(ok.map(_.auth), Right(GitAuth("x-token-auth", "bb_x")))
      assert(bad.isLeft)
    }
  }

  test("AWS SigV4: S3 以外はパスの各セグメントをもう一度打ち消す（S3 はそのまま）") {
    val t = java.time.Instant.parse("2015-08-30T12:36:00Z")
    def auth(svc: String) = SigV4.sign(
      "GET",
      org.http4s.Uri.unsafeFromString("https://x.amazonaws.com/functions/arn%3Aaws%3Alambda/invocations"),
      Map.empty,
      Array.emptyByteArray,
      "AK",
      "SK",
      None,
      "us-east-1",
      svc,
      t
    )("authorization")
    // 二重に打ち消すかどうかで署名が変わる（S3 は x-amz-content-sha256 も署名に入る）
    assertNotEquals(auth("lambda"), auth("s3"))
    assert(auth("s3").contains("SignedHeaders=host;x-amz-content-sha256;x-amz-date"), auth("s3"))
  }

  test("AWS SigV4: AWS の公開しているテストの値（get-vanilla）と同じ署名になる") {
    val h = SigV4.sign(
      "GET",
      org.http4s.Uri.unsafeFromString("https://example.amazonaws.com/"),
      Map.empty,
      Array.emptyByteArray,
      "AKIDEXAMPLE",
      "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY",
      None,
      "us-east-1",
      "service",
      java.time.Instant.parse("2015-08-30T12:36:00Z")
    )
    assertEquals(
      h.get("authorization"),
      Some("AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/service/aws4_request, " +
        "SignedHeaders=host;x-amz-date, Signature=5fa00fa31553b73ebf1942676e86291e8372ff2a2260956d9b8aae1d763fbf31")
    )
    assertEquals(h.get("x-amz-date"), Some("20150830T123600Z"))
  }

  test("クラウド: 資格情報は、そのクラウドの API のホストにだけ送る") {
    val cc = new CloudClients(org.http4s.client.Client[IO](_ =>
      cats.effect.Resource.eval(IO.raiseError(new Exception("送らない")))
    ))
    import skakapo.shared.CloudProvider.*
    def cred(p: String, cfg: Map[String, String] = Map.empty) =
      skakapo.shared.CloudCredentialDto(UUID.randomUUID(), "c", p, cfg, true, "", true)
    assert(cc.allowed(cred(Aws), "ec2.ap-northeast-1.amazonaws.com") &&
      !cc.allowed(cred(Aws), "amazonaws.com.evil.example") && !cc.allowed(cred(Aws), "example.com"))
    // だれでも作れるエンドポイント（API Gateway・S3 の静的サイト）には送らない
    assert(!cc.allowed(cred(Aws), "abc123.execute-api.us-east-1.amazonaws.com") &&
      !cc.allowed(cred(Aws), "b.s3-website-us-east-1.amazonaws.com"))
    // 他人のバケット（仮想ホスト形式）・OpenSearch のドメインなど、名前が前に付くホストも通さない. サービスの API は通す
    assert(!cc.allowed(cred(Aws), "evil-bucket.s3.amazonaws.com") &&
      !cc.allowed(cred(Aws), "search-x.us-east-1.es.amazonaws.com"))
    assert(cc.allowed(cred(Aws), "s3.ap-northeast-1.amazonaws.com") && cc.allowed(cred(Aws), "sts.amazonaws.com") &&
      cc.allowed(cred(Aws), "logs.us-gov-west-1.amazonaws.com"))
    assertEquals(cc.accountIdOf("arn:aws:iam::123456789012:user/skakapo"), Some("123456789012"))
    assert(cc.allowed(cred(Gcp), "storage.googleapis.com") && !cc.allowed(cred(Gcp), "googleapis.com.evil.example"))
    // 管理者が送り先を絞ったら、それ以外には送らない（* は使えるが、既定の範囲は超えない）
    val narrow = cred(Aws, Map("allowedHosts" -> "logs.*.amazonaws.com, ec2.ap-northeast-1.amazonaws.com"))
    assert(cc.allowed(narrow, "logs.us-east-1.amazonaws.com") && cc.allowed(narrow, "ec2.ap-northeast-1.amazonaws.com"))
    assert(!cc.allowed(narrow, "s3.us-east-1.amazonaws.com") &&
      !cc.allowed(cred(Aws, Map("allowedHosts" -> "*")), "example.com"))
    assertEquals(cc.azureScope("management.azure.com"), Some("https://management.azure.com/.default"))
    assertEquals(cc.azureScope("myvault.vault.azure.net"), Some("https://vault.azure.net/.default"))
    assertEquals(cc.azureScope("evil.example"), None)
    val aws = skakapo.shared.CloudCredentialDto(
      UUID.randomUUID(),
      "a",
      Aws,
      Map("accessKeyId" -> "AKID", "region" -> "us-east-1"),
      true,
      "",
      true
    )
    for {
      o1 <- cc.request(
        aws,
        """{"secretAccessKey":"s"}""",
        CloudCall("GET", "https://example.com/", Map.empty, "", Some("ec2"), None)
      )
      o2 <- cc.request(
        aws,
        """{"secretAccessKey":"s"}""",
        CloudCall("GET", "http://ec2.us-east-1.amazonaws.com/", Map.empty, "", Some("ec2"), None)
      )
      o3 <- cc.request(
        aws,
        """{"secretAccessKey":"s"}""",
        CloudCall("GET", "https://ec2.us-east-1.amazonaws.com/", Map.empty, "", None, None)
      )
      // アカウント ID のない（前に保存した）資格情報では、S3 に送らない（他人のバケットに書き出させない）
      o4 <- cc.request(
        aws,
        """{"secretAccessKey":"s"}""",
        CloudCall("PUT", "https://s3.us-east-1.amazonaws.com/b/k", Map.empty, "x", Some("s3"), None)
      )
    } yield assert(
      o1.isError && o2.isError && o3.isError && o3.text.contains("service") && o4.isError && o4.text.contains("保存し直して"),
      List(o1, o2, o3, o4)
    )
  }

  test("クラウド: AWS は SigV4 で署名し、GCP はサービスアカウントの鍵から発行したトークンを付ける（使い回す）") {
    import org.http4s.*
    import org.http4s.dsl.io.*
    val (pem, _) = rsaPem(false)
    val issued = new java.util.concurrent.atomic.AtomicInteger(0)
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[(String, Map[String, String])]()
    val app = HttpRoutes.of[IO] {
      case r @ POST -> Root / "token" =>
        r.as[UrlForm].flatMap { f =>
          assertEquals(f.getFirst("grant_type"), Some("urn:ietf:params:oauth:grant-type:jwt-bearer"))
          IO(issued.incrementAndGet()) *> Ok("""{"access_token":"ya29.x","expires_in":3600}""")
        }
      case r =>
        IO(seen.add(r.uri.toString -> r.headers.headers.map(h => h.name.toString.toLowerCase -> h.value).toMap)) *>
          Ok("ok")
    }
    val cc = new CloudClients(org.http4s.client.Client.fromHttpApp(app.orNotFound))
    val gcp = skakapo.shared.CloudCredentialDto(
      UUID.randomUUID(),
      "g",
      skakapo.shared.CloudProvider.Gcp,
      Map.empty,
      true,
      "",
      true
    )
    val key = Json.obj("client_email" -> "bot@p.iam.gserviceaccount.com".asJson, "private_key" -> pem.asJson).noSpaces
    val aws = skakapo.shared.CloudCredentialDto(
      UUID.randomUUID(),
      "a",
      skakapo.shared.CloudProvider.Aws,
      Map("accessKeyId" -> "AKID", "region" -> "ap-northeast-1"),
      true,
      "",
      true
    )
    for {
      g1 <- cc.request(
        gcp,
        Json.obj("serviceAccountJson" -> key.asJson).noSpaces,
        CloudCall("GET", "https://storage.googleapis.com/storage/v1/b?project=p", Map.empty, "", None, None)
      )
      g2 <- cc.request(
        gcp,
        Json.obj("serviceAccountJson" -> key.asJson).noSpaces,
        CloudCall("GET", "https://storage.googleapis.com/storage/v1/b?project=p", Map.empty, "", None, None)
      )
      a1 <- cc.request(
        aws,
        """{"secretAccessKey":"s","sessionToken":"tok"}""",
        CloudCall(
          "GET",
          "https://ec2.ap-northeast-1.amazonaws.com/?Action=DescribeInstances&Version=2016-11-15",
          Map.empty,
          "",
          Some("ec2"),
          None
        )
      )
      s3 <- cc.request(
        aws.copy(config = aws.config + ("accountId" -> "123456789012")),
        """{"secretAccessKey":"s"}""",
        CloudCall("PUT", "https://s3.ap-northeast-1.amazonaws.com/some-bucket/k", Map.empty, "x", Some("s3"), None)
      )
    } yield {
      assert(!g1.isError && !g2.isError && !a1.isError, List(g1, g2, a1))
      assertEquals(issued.get(), 1)
      val reqs = seen.asScala.toList
      assertEquals(reqs.head._2.get("authorization"), Some("Bearer ya29.x"))
      val awsAuth = reqs.find(_._1.contains("ec2.")).map(_._2.getOrElse("authorization", "")).getOrElse("")
      assert(
        awsAuth.startsWith("AWS4-HMAC-SHA256 Credential=AKID/") &&
          awsAuth.contains("/ap-northeast-1/ec2/aws4_request") &&
          awsAuth.contains("SignedHeaders=host;x-amz-date;x-amz-security-token"),
        awsAuth
      )
      val ec2 = reqs.find(_._1.contains("ec2.")).get._2
      assertEquals(ec2.get("x-amz-security-token"), Some("tok"))
      // S3 は自分のアカウントのバケットだけ（持ち主の指定を付け、署名にも含める）
      val s3h = reqs.find(_._1.contains("s3."))
      assert(!s3.isError, s3)
      assertEquals(s3h.flatMap(_._2.get("x-amz-expected-bucket-owner")), Some("123456789012"))
      assert(s3h.exists(_._2.getOrElse("authorization", "").contains("x-amz-expected-bucket-owner")), s3h)
    }
  }
}

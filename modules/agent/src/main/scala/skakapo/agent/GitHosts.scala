package skakapo.agent

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{Accept, Authorization}
import skakapo.shared.RepositoryDto

import scala.concurrent.duration.*

/** Git のホストの API（プルリクエストとコメント）. GitHub（GitHub Enterprise）・Bitbucket Cloud・Forgejo. */
object GitHosts {

  /** 共有アカウントの API の場所（空なら既定. Forgejo はリポジトリのホストの /api/v1）. */
  def apiFor(
    provider: String,
    apiUrl: String,
    repoUrl: String
  ): String =
    if apiUrl.nonEmpty then apiUrl.stripSuffix("/")
    else if provider == skakapo.shared.GitProvider.Forgejo then GitAuth.origin(repoUrl).fold("")(_ + "/api/v1")
    else skakapo.shared.GitProvider.defaultApi(provider)
}

final class GitHosts(
  http: Client[IO]) {

  /** GitHub App・Bitbucket の OAuth コンシューマーのトークン. */
  val apps: GitApps = new GitApps(http, this)

  private val MaxBody = 2L * 1024 * 1024

  /** https://host/owner/repo(.git) から (owner, repo). */
  def ownerRepo(
    url: String
  ): Option[(String, String)] =
    "^https://[^/@]+/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+?)(?:\\.git)?/?$".r.findFirstMatchIn(url.trim).map(m =>
      m.group(1) -> m.group(2)
    )

  /** GitHub のサービスの URL（https://github.com・GitHub Enterprise）から API の場所. */
  def apiBase(
    serviceBase: String
  ): String = {
    val b = serviceBase.stripSuffix("/")
    if b.isEmpty || b == "https://github.com" then "https://api.github.com" else s"$b/api/v3"
  }

  private def call(
    c: RepoCreds,
    method: Method,
    url: String,
    body: Json = Json.Null
  ): IO[(Status, Json)] = {
    val auth = c.provider match {
      // Bitbucket: アクセストークン（x-token-auth）は Bearer、app password は Basic（ユーザー名:パスワード）
      case skakapo.shared.GitProvider.Bitbucket if c.auth.username != "x-token-auth" =>
        Authorization(BasicCredentials(c.auth.username, c.auth.secret))
      // Forgejo（Gitea 互換）のアクセストークンは「token」の方式
      case skakapo.shared.GitProvider.Forgejo =>
        Authorization(Credentials.Token(org.typelevel.ci.CIString("token"), c.auth.secret))
      case _ => Authorization(Credentials.Token(AuthScheme.Bearer, c.auth.secret))
    }
    send(method, url, auth, c.provider == skakapo.shared.GitProvider.Github, body)
  }

  /** Git のホストの API を呼ぶ（GitHosts と GitApps の共通. 応答の JSON は 2MB まで）. */
  private[agent] def send(
    method: Method,
    url: String,
    auth: Authorization,
    github: Boolean,
    body: Json = Json.Null
  ): IO[(Status, Json)] = {
    val req0 = Request[IO](method, Uri.unsafeFromString(url))
    val req = (if body.isNull then req0 else req0.withEntity(body))
      .putHeaders(
        auth,
        Accept(MediaType.unsafeParse(if github then "application/vnd.github+json" else "application/json")),
        "X-GitHub-Api-Version" -> "2022-11-28",
        "User-Agent" -> "skakapo"
      )
    http.run(req).use(r =>
      HttpBody.read(r, MaxBody)
        .map(t => r.status -> io.circe.parser.parse(t).getOrElse(Json.obj()))
    ).timeout(60.seconds)
  }

  private[agent] def message(
    j: Json
  ): String =
    (j.hcursor.get[String]("message").toOption.toList ++
      j.hcursor.downField("error").get[String]("message").toOption.toList ++
      j.hcursor.downField("errors").as[List[Json]].getOrElse(Nil).flatMap(e =>
        e.hcursor.get[String]("message").toOption
      )).mkString(" / ").take(500)

  private def withRepo(
    repo: RepositoryDto
  )(
    f: (String, String) => IO[Out]
  ): IO[Out] =
    ownerRepo(repo.url).fold(IO.pure(Out.err(s"${repo.url} からオーナーとリポジトリを読み取れません")))(f.tupled)

  def createPullRequest(
    c: RepoCreds,
    repo: RepositoryDto,
    title: String,
    body: String,
    head: String,
    base: String,
    draft: Boolean
  ): IO[Out] =
    withRepo(repo) {
      (
        o,
        r
      ) =>
        c.provider match {
          // Forgejo の API は GitHub と同じ形. 下書きの項目はないので、題名の先頭を WIP: にする
          case skakapo.shared.GitProvider.Github | skakapo.shared.GitProvider.Forgejo =>
            val forgejo = c.provider == skakapo.shared.GitProvider.Forgejo
            val t = if forgejo && draft then s"WIP: $title" else title
            call(
              c,
              Method.POST,
              s"${c.apiBase}/repos/$o/$r/pulls",
              Json.obj(
                "title" -> t.asJson,
                "body" -> body.asJson,
                "head" -> head.asJson,
                "base" -> base.asJson
              ).deepMerge(if forgejo then Json.obj() else Json.obj("draft" -> draft.asJson))
            ).map {
              (
                st,
                j
              ) =>
                if st.isSuccess then
                  Out(
                    s"プルリクエストを出しました: ${j.hcursor.get[String]("html_url").getOrElse("")}（#${j.hcursor.get[Int]("number").getOrElse(0)}）"
                  )
                else Out.err(s"プルリクエストを出せませんでした（${st.code}）: ${message(j)}")
            }
          case skakapo.shared.GitProvider.Bitbucket =>
            call(
              c,
              Method.POST,
              s"${c.apiBase}/repositories/$o/$r/pullrequests",
              Json.obj(
                "title" -> title.asJson,
                "description" -> body.asJson,
                "source" -> Json.obj("branch" -> Json.obj("name" -> head.asJson)),
                "destination" -> Json.obj("branch" -> Json.obj("name" -> base.asJson)),
                "draft" -> draft.asJson
              )
            ).map {
              (
                st,
                j
              ) =>
                if st.isSuccess then
                  Out(
                    s"プルリクエストを出しました: ${j.hcursor.downField("links").downField("html").get[String]("href").getOrElse("")}（#${j.hcursor.get[Int]("id").getOrElse(0)}）"
                  )
                else Out.err(s"プルリクエストを出せませんでした（${st.code}）: ${message(j)}")
            }
          case _ => IO.pure(Out.err("このリポジトリのホストでは、プルリクエストを出せません（push までです）"))
        }
    }

  def comment(
    c: RepoCreds,
    repo: RepositoryDto,
    number: Int,
    body: String
  ): IO[Out] =
    withRepo(repo) {
      (
        o,
        r
      ) =>
        c.provider match {
          case skakapo.shared.GitProvider.Github | skakapo.shared.GitProvider.Forgejo =>
            // issues の API はプルリクエストでない issue にも書けるので、先にプルリクエストであることを確かめる
            call(c, Method.GET, s"${c.apiBase}/repos/$o/$r/pulls/$number").flatMap {
              (
                pst,
                _
              ) =>
                if !pst.isSuccess then IO.pure(Out.err(s"プルリクエスト #$number が見つかりません（${pst.code}）"))
                else
                  call(
                    c,
                    Method.POST,
                    s"${c.apiBase}/repos/$o/$r/issues/$number/comments",
                    Json.obj("body" -> body.asJson)
                  ).map {
                    (
                      st,
                      j
                    ) =>
                      if st.isSuccess then Out(s"コメントしました: ${j.hcursor.get[String]("html_url").getOrElse("")}")
                      else Out.err(s"コメントできませんでした（${st.code}）: ${message(j)}")
                  }
            }
          case skakapo.shared.GitProvider.Bitbucket =>
            call(
              c,
              Method.POST,
              s"${c.apiBase}/repositories/$o/$r/pullrequests/$number/comments",
              Json.obj("content" -> Json.obj("raw" -> body.asJson))
            ).map {
              (
                st,
                j
              ) =>
                if st.isSuccess then
                  Out(s"コメントしました: ${j.hcursor.downField("links").downField("html").get[String]("href").getOrElse("")}")
                else Out.err(s"コメントできませんでした（${st.code}）: ${message(j)}")
            }
          case _ => IO.pure(Out.err("このリポジトリのホストでは、プルリクエストにコメントできません"))
        }
    }
}

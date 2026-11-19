package skakapo.agent

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import skakapo.shared.{GitAccountDto, GitAuthKind, RepositoryDto}

import scala.concurrent.duration.*

/** 発行したトークン（expiresAt はミリ秒）と、コミットの作者の既定（GitHub App のボット）. */
final case class AppToken(auth: GitAuth, expiresAt: Long, author: Option[(String, String)])

/**
 * GitHub App・Bitbucket の OAuth コンシューマーの、短い期限のトークンを発行する. 期限の5分前までは覚えたものを使い回す.
 * GitHub App のトークンは、そのリポジトリと Contents・Pull requests の書き込みだけに絞って発行する（Bitbucket は絞れない）.
 */
final class GitApps(http: Client[IO], hosts: GitHosts) {

  /** (アカウント, リポジトリ) → トークン. Bitbucket のトークンはリポジトリに縛られないので、リポジトリは空で覚える */
  private val cache = new TokenCache[(java.util.UUID, String), AppToken]

  /** (API の場所, App ID) → ボットの名前とメール（GitHub Enterprise と github.com で App ID が重なってもよいように） */
  private val bots = Ref.unsafe[IO, Map[(String, String), (String, String)]](Map.empty)

  private def call(method: Method, url: String, auth: Authorization, body: Json = Json.Null): IO[(Status, Json)] =
    hosts.send(method, url, auth, github = true, body)

  private def jwtAuth(a: GitAccountDto, pem: String): IO[Either[String, Authorization]] =
    IO.realTime.map(t =>
      AppJwt.privateKey(pem).map(k =>
        Authorization(Credentials.Token(AuthScheme.Bearer, AppJwt.sign(a.appId, k, t.toSeconds)))
      )
    )

  /** 鍵・シークレットでつながるか（保存する前の確かめ）. 表示用の名前（GitHub App の slug など）を返す. */
  def probe(a: GitAccountDto, secret: String, apiBase: String): IO[Either[String, String]] = a.authKind match {
    case GitAuthKind.GithubApp =>
      jwtAuth(a, secret).flatMap(_.flatTraverse(auth =>
        call(Method.GET, s"$apiBase/app", auth).map {
          (st, j) =>
            if st.isSuccess then Right(j.hcursor.get[String]("slug").getOrElse(a.appId))
            else Left(s"GitHub App として認証できませんでした（${st.code}）: ${hosts.message(j)}。App ID と秘密鍵を確かめてください")
        }
      )).handleError(e => Left(s"GitHub に接続できませんでした（${e.getClass.getSimpleName}）"))
    case GitAuthKind.BitbucketOAuth => bitbucket(a, secret, apiBase).map(_.as(a.appId))
    case _ => IO.pure(Right(a.name))
  }

  /** そのリポジトリに使うトークン（覚えていればそれを使う）. */
  def token(a: GitAccountDto, secret: String, repo: RepositoryDto, apiBase: String): IO[Either[String, AppToken]] = {
    val (key, issue) = a.authKind match {
      case GitAuthKind.GithubApp => ((a.id, repo.name), github(a, secret, repo, apiBase))
      case GitAuthKind.BitbucketOAuth => ((a.id, ""), bitbucket(a, secret, apiBase))
      case k => ((a.id, ""), IO.pure(Left(s"認証の方式「$k」ではトークンを発行しません")))
    }
    cache.get(key)(issue.map(_.map(t => t -> t.expiresAt)).handleError(e =>
      Left(s"トークンを発行できませんでした（${e.getClass.getSimpleName}）")
    ))
  }

  /** 秘密が変わったとき（保存・削除したとき）に、覚えたトークンを捨てる. */
  def forget(accountId: java.util.UUID): IO[Unit] = cache.forget(_._1 == accountId)

  private def github(a: GitAccountDto,
                     pem: String,
                     repo: RepositoryDto,
                     apiBase: String): IO[Either[String, AppToken]] =
    hosts.ownerRepo(repo.url) match {
      case None => IO.pure(Left(s"${repo.url} からオーナーとリポジトリを読み取れません"))
      case Some((owner, name)) =>
        jwtAuth(a, pem).flatMap(_.flatTraverse { jwt =>
          // Installation ID が空なら、リポジトリに入れたインストールを探す
          val installation: IO[Either[String, String]] =
            if a.installationId.nonEmpty then IO.pure(Right(a.installationId))
            else
              call(Method.GET, s"$apiBase/repos/$owner/$name/installation", jwt).map {
                (st, j) =>
                  j.hcursor.get[Long]("id").toOption.filter(_ => st.isSuccess).map(_.toString)
                    .toRight(s"$owner/$name に GitHub App が入っていません（${st.code}）。リポジトリ（または組織）に App をインストールしてください")
              }
          installation.flatMap(_.flatTraverse { inst =>
            call(
              Method.POST,
              s"$apiBase/app/installations/$inst/access_tokens",
              jwt,
              Json.obj(
                "repositories" -> List(name).asJson,
                "permissions" -> Json.obj("contents" -> "write".asJson, "pull_requests" -> "write".asJson)
              )
            ).flatMap {
              (st, j) =>
                (
                  j.hcursor.get[String]("token").toOption.filter(_ => st.isSuccess),
                  j.hcursor.get[String]("expires_at").toOption
                ) match {
                  case (Some(t), exp) =>
                    val until =
                      exp.flatMap(e => Either.catchNonFatal(java.time.Instant.parse(e).toEpochMilli).toOption)
                    bot(a, jwt, apiBase).map(author =>
                      Right(AppToken(
                        GitAuth("x-access-token", t),
                        until.getOrElse(System.currentTimeMillis() + 50.minutes.toMillis),
                        author
                      ))
                    )
                  case _ => IO.pure(Left(s"GitHub App のトークンを発行できませんでした（${st.code}）: ${hosts.message(j)}。" +
                      "App に Contents と Pull requests の Read and write の権限があり、このリポジトリに入っているか確かめてください"))
                }
            }
          })
        })
    }

  /** GitHub App のボット（コミットの作者の既定）: <slug>[bot] と、GitHub が結び付ける noreply のメール. */
  private def bot(a: GitAccountDto, jwt: Authorization, apiBase: String): IO[Option[(String, String)]] =
    bots.get.map(_.get(apiBase -> a.appId)).flatMap {
      case some @ Some(_) => IO.pure(some)
      case None =>
        (for {
          app <- call(Method.GET, s"$apiBase/app", jwt)
          slug = app._2.hcursor.get[String]("slug").toOption
          user <- slug.traverse(s => call(Method.GET, s"$apiBase/users/${Uri.pathEncode(s + "[bot]")}", jwt))
          id = user.flatMap(_._2.hcursor.get[Long]("id").toOption)
          bot = slug.map(s =>
            s"$s[bot]" -> id.fold(s"$s[bot]@users.noreply.github.com")(i => s"$i+$s[bot]@users.noreply.github.com")
          )
          _ <- bot.traverse_(b => bots.update(_ + ((apiBase -> a.appId) -> b)))
        } yield bot).handleError(_ => None)
    }

  /**
   * Bitbucket Cloud の OAuth コンシューマー（client credentials）. API が api.bitbucket.org なら、トークンの入口は bitbucket.org.
   * このトークンはリポジトリに絞れない（コンシューマーの権限がそのまま付く）. 権限はコンシューマーの設定で絞る.
   */
  private def bitbucket(a: GitAccountDto, secret: String, apiBase: String): IO[Either[String, AppToken]] = {
    val site = GitAuth.origin(apiBase).map(_.replace("://api.", "://")).getOrElse("https://bitbucket.org")
    (
      OAuthToken.request(
        http,
        s"$site/site/oauth2/access_token",
        UrlForm("grant_type" -> "client_credentials"),
        Some(Authorization(BasicCredentials(a.appId, secret)))
      ),
      IO.realTime
    ).mapN {
      (r, now) =>
        r.map(t => AppToken(GitAuth("x-token-auth", t.access), t.expiresAt(now.toMillis), None))
          .leftMap(e =>
            s"Bitbucket の OAuth コンシューマーで認証できませんでした: $e。キーとシークレット、" +
              "コンシューマーの「This is a private consumer」と権限（Repositories・Pull requests の Write）を確かめてください"
          )
    }
  }
}

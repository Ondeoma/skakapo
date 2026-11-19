package skakapo.agent

import cats.effect.*
import io.circe.Json
import io.circe.parser.parse
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.{Accept, Authorization}

import scala.concurrent.duration.*

/** OAuth のトークンの入口に頼む（認可コード・refresh token・client credentials・JWT bearer の共通）. 応答は 1MB まで. */
object OAuthToken {
  def request(
    http: Client[IO],
    url: String,
    form: UrlForm,
    auth: Option[Authorization] = None
  ): IO[Either[String, TokenResponse]] =
    Uri.fromString(url).fold(
      _ => IO.pure(Left(s"トークンの入口の URL が正しくありません: $url")),
      uri => {
        val req = Request[IO](Method.POST, uri).withEntity(form).putHeaders(Accept(MediaType.application.json))
        http.run(
          auth.fold(req)(req.putHeaders(_))
        ).use(r => HttpBody.read(r, 1024 * 1024).map(r.status -> _)).timeout(30.seconds).attempt.map {
          case Left(e) => Left(s"トークンの入口に接続できませんでした（${e.getClass.getSimpleName}）")
          case Right((st, body)) =>
            val c = parse(body).getOrElse(Json.obj()).hcursor
            c.get[String]("access_token").toOption.filter(t => t.nonEmpty && st.isSuccess)
              .map(t =>
                TokenResponse(
                  t,
                  c.get[String]("refresh_token").toOption,
                  c.get[String]("scope").getOrElse(""),
                  c.get[Long]("expires_in").toOption
                )
              )
              .toRight(
                s"トークンを受け取れませんでした（${st.code} ${c.get[String]("error_description").orElse(c.get[String]("error")).getOrElse("").take(300)}）"
              )
        }
      }
    )
}

package skakapo.agent

import cats.effect.*
import io.circe.Json
import io.circe.parser.parse
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.{Accept, Authorization}

import scala.concurrent.duration.*

/** OAuth のトークンの入口の応答. expiresIn は秒. */
final case class TokenResponse(access: String, refresh: Option[String], scope: String, expiresIn: Option[Long]):
  /** 期限（ミリ秒）. 応答に期限がなければ1時間. */
  def expiresAt(nowMillis: Long): Long = nowMillis + expiresIn.getOrElse(3600L) * 1000

/** OAuth のトークンの入口に頼む（認可コード・refresh token・client credentials・JWT bearer の共通）. 応答は 1MB まで. */
object OAuthToken:
  def request(http: Client[IO], url: String, form: UrlForm, auth: Option[Authorization] = None): IO[Either[String, TokenResponse]] =
    Uri.fromString(url).fold(_ => IO.pure(Left(s"トークンの入口の URL が正しくありません: $url")), uri => {
    val req = Request[IO](Method.POST, uri).withEntity(form).putHeaders(Accept(MediaType.application.json))
    http.run(auth.fold(req)(req.putHeaders(_))).use(r => HttpBody.read(r, 1024 * 1024).map(r.status -> _)).timeout(30.seconds).attempt.map {
      case Left(e) => Left(s"トークンの入口に接続できませんでした（${e.getClass.getSimpleName}）")
      case Right((st, body)) =>
        val c = parse(body).getOrElse(Json.obj()).hcursor
        c.get[String]("access_token").toOption.filter(t => t.nonEmpty && st.isSuccess)
          .map(t => TokenResponse(t, c.get[String]("refresh_token").toOption, c.get[String]("scope").getOrElse(""), c.get[Long]("expires_in").toOption))
          .toRight(s"トークンを受け取れませんでした（${st.code} ${c.get[String]("error_description").orElse(c.get[String]("error")).getOrElse("").take(300)}）")
    }})

/**
 * 期限つきの値（短い期限のトークン・ツールの一覧など）を覚える. 期限の margin 前までは使い回し、それより近ければ作り直す
 * （1000件を超えたら全部忘れる）.
 */
final class TokenCache[K, V](margin: FiniteDuration = 5.minutes):
  private val ref = Ref.unsafe[IO, Map[K, (Long, V)]](Map.empty)

  /** 覚えていればそれを、なければ issue で発行して覚える（issue は値と期限（ミリ秒）を返す）. */
  def get(key: K)(issue: IO[Either[String, (V, Long)]]): IO[Either[String, V]] =
    IO.realTime.map(_.toMillis).flatMap { now =>
      ref.get.map(_.get(key).filter(_._1 - margin.toMillis > now)).flatMap {
        case Some((_, v)) => IO.pure(Right(v))
        case None => issue.flatMap {
            case Right((v, exp)) => ref.update(m => (if m.size > 1000 then Map.empty else m) + (key -> (exp, v))).as(Right(v))
            case Left(e)         => IO.pure(Left(e))
          }
      }
    }

  /** 当てはまるものを忘れる（秘密を変えた・消したとき）. */
  def forget(p: K => Boolean): IO[Unit] = ref.update(_.filterNot((k, _) => p(k)))

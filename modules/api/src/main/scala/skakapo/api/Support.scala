package skakapo.api

import cats.effect.*
import org.http4s.*
import skakapo.db.*
import skakapo.shared.*

import java.util.UUID
import scala.concurrent.duration.*

final case class ApiConfig(cookieSecure: Boolean,
                           publicUrl: String,
                           sessionTtl: FiniteDuration = 14.days,
                           cookieName: String = "skakapo_session",
                           devLogin: Boolean = false,
                           rateLimit: RateLimitConfig = RateLimitConfig(),
                           version: String = "0.1.0",
                           adminGroups: Set[String] = Set.empty,
                           agentReady: Boolean = false)

/** 誰の要求か. token は API トークンで呼ばれたときの (トークンID, スコープ). */
final case class Principal(user: User, token: Option[(UUID, String)]) {
  def canWrite: Boolean = token.forall(_._2 == TokenScope.Write)
}

/** 文字列の応答（JSON 以外. circe の EntityEncoder に取られないよう明示する）. */
object TextResponse {
  def apply(status: Status, body: String, media: MediaType = MediaType.text.plain): Response[IO] =
    Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder).withContentType(headers.`Content-Type`(
      media,
      Charset.`UTF-8`
    ))
}

/** 入力の確認. */
object Check {

  /** 名前・題名（前後の空白を除いて1〜100文字）. */
  def name(v: String, what: String = "名前"): Either[String, String] =
    Either.cond(v.trim.nonEmpty && v.trim.length <= 100, v.trim, s"${what}を100文字以内で入れてください")

  def text(v: String, what: String, max: Int): Either[String, String] =
    Either.cond(v.length <= max, v, s"$what は $max 文字以内にしてください")
}

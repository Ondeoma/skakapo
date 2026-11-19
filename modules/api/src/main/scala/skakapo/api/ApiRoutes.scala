package skakapo.api

import cats.data.OptionT
import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.server.middleware.EntityLimiter
import skakapo.core.ErrorCode
import skakapo.db.*
import sbirds.http.Errors
import skakapo.shared.*

/** アプリのルートをまとめる（REST API・MCP・プッシュ通知の購読）. MCP は REST API をトークンで内部から呼ぶ. */
object ApiRoutes {
  def apply(
    oidc: Option[sbirds.auth.OidcClient],
    users: UserStore,
    sess: SessionAuth,
    cfg: ApiConfig,
    appRoutes: HttpRoutes[IO],
    push: Option[(sbirds.push.PushStore, sbirds.push.PushService)] = None
  ): HttpRoutes[IO] = {
    val rest = new AuthRoutes(oidc, users, sess, cfg).routes <+> appRoutes <+>
      push.fold(HttpRoutes.empty[IO])(
        (
          store,
          service
        ) => new PushRoutes(store, service, sess).routes
      )
    // 本文は MaxBody まで（大きな本文でメモリを使い切らせない. 添付するファイルの上限より大きくしておく）.
    // Content-Length が超えていれば、EntityLimiter はルートに入る前に例外にするので、ここで 413 にする
    val limited = EntityLimiter.httpRoutes(rest <+> new McpRoutes(rest, sess, cfg.version).routes, MaxBody)
    val all = HttpRoutes[IO](req =>
      limited(req).recoverWith { case _: EntityLimiter.EntityTooLarge => OptionT.liftF(tooLarge) }
    )
    // セキュリティのヘッダーは、起動するところ（Main）でアプリ全体に付ける
    RateLimit(all, cfg.rateLimit)
  }

  /** 本文の上限（添付する画像の上限より少し大きく） */
  val MaxBody: Long = AttachmentRule.MaxBytes.toLong + 1024 * 1024

  private def tooLarge: IO[Response[IO]] = Errors.respond(ErrorCode.RequestTooLarge)
}

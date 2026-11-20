package skakapo.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import skakapo.core.ErrorCode
import skakapo.db.*
import sbirds.http.BodyLimit
import skakapo.shared.*

/** アプリのルートをまとめる（REST API・MCP・プッシュ通知の購読）. MCP は REST API をトークンで内部から呼ぶ. */
object ApiRoutes {
  def apply(oidc: Option[sbirds.auth.OidcClient],
            users: UserStore,
            sess: SessionAuth,
            cfg: ApiConfig,
            appRoutes: HttpRoutes[IO],
            push: Option[(sbirds.push.PushStore, sbirds.push.PushService)] = None): HttpRoutes[IO] = {
    val rest = new AuthRoutes(oidc, users, sess, cfg).routes <+> appRoutes <+>
      push.fold(HttpRoutes.empty[IO])((store, service) => new PushRoutes(store, service, sess).routes)
    // 本文は MaxBody まで（大きな本文でメモリを使い切らせない. 添付するファイルの上限より大きくしておく）. 超えていれば 413
    val all = BodyLimit(MaxBody, ErrorCode.RequestTooLarge)(rest <+> new McpRoutes(rest, sess, cfg.version).routes)
    // セキュリティのヘッダーは、起動するところ（Main）でアプリ全体に付ける
    // IP ごとの回数の制限（数え方は全サービス共通. sbirds-lib の RateLimit.standard）
    sbirds.http.RateLimit.standard(all, cfg.rateLimit, ErrorCode.TooManyRequests)()
  }

  /** 本文の上限（添付する画像の上限より少し大きく） */
  val MaxBody: Long = AttachmentRule.MaxBytes.toLong + 1024 * 1024
}

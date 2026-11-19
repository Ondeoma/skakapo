package skakapo.api

import cats.effect.*
import org.http4s.*
import skakapo.core.ErrorCode
import sbirds.http.Errors
import sbirds.shared.AppError

/** ブラウザで直接開く、短い案内のページ（ログイン・連携の失敗など）. */
object HtmlPage {
  private def esc(s: String) = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")
  def apply(title: String, msg: String, status: Status, backHref: String, backLabel: String): IO[Response[IO]] =
    IO.pure(TextResponse(
      status,
      s"""<!doctype html><html lang="ja"><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>${esc(
          title
        )}</title>
         |<body style="font-family:system-ui,sans-serif;max-width:480px;margin:12vh auto;padding:0 1rem">
         |<h1 style="font-size:1.3rem">${esc(title)}</h1><p>${esc(msg)}</p><p><a href="${esc(backHref)}">${esc(
          backLabel
        )}</a></p></body></html>""".stripMargin,
      MediaType.text.html
    ))

  /**
   * エラーの案内のページ（コード付きでログに出し、ページにもコードを出す）. detail を省くとコードの既定の文言.
   * logDetail はログにだけ出す（IdP からの説明など、URL で差し替えられて画面には出せないもの）.
   */
  def error(title: String,
            code: ErrorCode,
            backHref: String,
            backLabel: String,
            detail: String | Null = null,
            logDetail: String = ""): IO[Response[IO]] = {
    val shown = Option(detail).getOrElse(code.message)
    Errors.log(AppError(code, Some(s"$shown $logDetail".trim))) *>
      apply(
        title,
        s"$shown（${code.code}）",
        Status.fromInt(code.status).getOrElse(Status.BadRequest),
        backHref,
        backLabel
      )
  }
}

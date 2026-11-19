package skakapo.core

object Text {

  /** n 文字を超えたら切り、省略したことを書き添える. */
  def clip(s: String, n: Int, hint: String = ""): String =
    if (s.length <= n) s else s.take(n) + s"\n…（${s.length - n} 文字を省略${if (hint.nonEmpty) s". $hint" else ""}）"

  /** 例外の説明（なければ空. n 文字まで）. */
  def errMsg(e: Throwable, n: Int): String = Option(e.getMessage).getOrElse("").take(n)
}

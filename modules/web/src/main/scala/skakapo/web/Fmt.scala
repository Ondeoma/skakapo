package skakapo.web

import scala.scalajs.js

/** 表示の書式. */
object Fmt {
  private def pad(n: Double, w: Int = 2): String = { val s = n.toInt.toString; "0" * math.max(0, w - s.length) + s }

  /** 日時（ISO 8601）をこの端末の時刻で（2026-11-17 09:08）. */
  def dateTime(iso: String): String = {
    val d = new js.Date(iso)
    if (d.getTime().isNaN) iso
    else
      s"${d.getFullYear().toInt}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}"
  }

  def time(iso: String): String = {
    val d = new js.Date(iso); s"${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}"
  }

  /** 「3分前」など. */
  def ago(iso: String): String = {
    val diff = (js.Date.now() - js.Date.parse(iso)) / 1000
    if (diff < 60) "たった今"
    else if (diff < 3600) s"${(diff / 60).toInt}分前"
    else if (diff < 86400) s"${(diff / 3600).toInt}時間前"
    else if (diff < 86400 * 30) s"${(diff / 86400).toInt}日前"
    else dateTime(iso).take(10)
  }

  /** 期間（3時間12分）. to が None なら今まで. */
  def span(from: String, to: Option[String]): String = {
    val sec = ((to.map(js.Date.parse).getOrElse(js.Date.now()) - js.Date.parse(from)) / 1000).toLong.max(0)
    if (sec < 60) s"${sec}秒"
    else if (sec < 3600) s"${sec / 60}分"
    else if (sec < 86400) s"${sec / 3600}時間${(sec % 3600) / 60}分"
    else s"${sec / 86400}日${(sec % 86400) / 3600}時間"
  }

  /** 状態の色の class. */
  def statusClass(s: String): String = s"st st-$s"

  /** トークンの数（1.2k・3.4M）. */
  def tokens(n: Long): String =
    if (n < 1000) n.toString else if (n < 1000000) f"${n / 1000.0}%.1fk" else f"${n / 1000000.0}%.1fM"
}

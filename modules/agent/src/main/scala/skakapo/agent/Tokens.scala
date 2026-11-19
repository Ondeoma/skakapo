package skakapo.agent

/** OAuth のトークンの入口の応答. expiresIn は秒. */
final case class TokenResponse(
  access: String,
  refresh: Option[String],
  scope: String,
  expiresIn: Option[Long]) {

  /** 期限（ミリ秒）. 応答に期限がなければ1時間. */
  def expiresAt(
    nowMillis: Long
  ): Long = nowMillis + expiresIn.getOrElse(3600L) * 1000
}

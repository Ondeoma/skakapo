package skakapo.agent

/** cloud_request の引数. */
final case class CloudCall(
  method: String,
  url: String,
  headers: Map[String, String],
  body: String,
  service: Option[String],
  region: Option[String])

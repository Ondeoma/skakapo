package skakapo.core

import io.circe.Json
import io.circe.syntax.*

/** ツールの入力の JSON Schema を組む部品（Claude に渡すツールと、skakapo の MCP サーバーで共有）. */
object JsonSchema {
  def obj(required: List[String], props: (String, Json)*): Json =
    Json.obj("type" -> "object".asJson, "properties" -> Json.obj(props*), "required" -> required.asJson)

  private def typed(t: String, d: String): Json = Json.obj("type" -> t.asJson, "description" -> d.asJson)
  def str(d: String): Json = typed("string", d)
  def int(d: String): Json = typed("integer", d)
  def bool(d: String): Json = typed("boolean", d)
  def anyObj(d: String): Json = typed("object", d)
  def oneOf(d: String, vs: Seq[String]): Json =
    Json.obj("type" -> "string".asJson, "enum" -> vs.toList.asJson, "description" -> d.asJson)
}

package skakapo.agent

import com.anthropic.core.{JsonValue, ObjectMappers}
import com.anthropic.models.messages.*
import com.fasterxml.jackson.core.`type`.TypeReference
import io.circe.Json
import io.circe.parser.parse

import scala.jdk.CollectionConverters.*

/** circe の JSON と、Anthropic の SDK（Jackson）の値の変換. */
object Jsons {
  private val mapper = ObjectMappers.jsonMapper()

  def toJsonValue(
    j: Json
  ): JsonValue = JsonValue.fromJsonNode(mapper.readTree(j.noSpaces))

  def fromJsonValue(
    v: JsonValue
  ): Json = parse(mapper.writeValueAsString(v)).getOrElse(Json.Null)

  /** 会話（MessageParam の配列）を JSON にする（DB に保存して、別のノードが続きから読めるように）. */
  def encodeTranscript(
    ms: List[MessageParam]
  ): Json = parse(mapper.writeValueAsString(ms.asJava)).getOrElse(Json.arr())

  def decodeTranscript(
    j: Json
  ): List[MessageParam] =
    mapper.readValue(j.noSpaces, new TypeReference[java.util.List[MessageParam]]() {}).asScala.toList

  /** JSON Schema（MCP の inputSchema）を、Claude のツールの入力スキーマにする. */
  def inputSchema(
    schema: Json
  ): Tool.InputSchema = {
    val c = schema.hcursor
    val props = Tool.InputSchema.Properties.builder()
    c.downField("properties").focus.flatMap(_.asObject).foreach(_.toList.foreach(
      (
        k,
        v
      ) => props.putAdditionalProperty(k, toJsonValue(v))
    ))
    val b = Tool.InputSchema.builder().properties(props.build())
    c.get[List[String]]("required").toOption.filter(_.nonEmpty).foreach(r => b.required(r.asJava))
    b.build()
  }
}

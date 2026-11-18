package skakapo.agent

import io.circe.Json
import io.circe.syntax.*

/** 会話（MessageParam の配列の JSON）の手入れ. */
object Transcript:

  private val Unknown = "処理の途中で止まったため、このツールの結果は分かりません。必要なら確かめ直してください。"

  private def blocks(m: Json): List[Json] = m.hcursor.downField("content").as[List[Json]].getOrElse(Nil)
  private def role(m: Json): Option[String] = m.hcursor.get[String]("role").toOption
  private def toolUseIds(m: Json): List[String] =
    blocks(m).filter(_.hcursor.get[String]("type").contains("tool_use")).flatMap(_.hcursor.get[String]("id").toOption)
  private def toolResultIds(m: Json): Set[String] =
    blocks(m).filter(_.hcursor.get[String]("type").contains("tool_result")).flatMap(_.hcursor.get[String]("tool_use_id").toOption).toSet

  private def result(id: String): Json =
    Json.obj("type" -> "tool_result".asJson, "tool_use_id" -> id.asJson, "content" -> Unknown.asJson, "is_error" -> true.asJson)

  /**
   * 結果のない tool_use に「結果は分からない」を補う（Claude API は、tool_use の直後の利用者の発言に、すべての結果を求める）.
   * 会話の途中（ノードの停止・途中で切れた応答のあとに利用者が返事をした）でも、全体を見て直す.
   */
  def repair(t: Json): Json =
    val ms = t.asArray.map(_.toList).getOrElse(Nil)
    val out = List.newBuilder[Json]
    var i = 0
    while i < ms.size do
      val m = ms(i)
      out += m
      val ids = if role(m).contains("assistant") then toolUseIds(m) else Nil
      if ids.nonEmpty then
        val next = ms.lift(i + 1).filter(n => role(n).contains("user"))
        val have = next.map(toolResultIds).getOrElse(Set.empty)
        val missing = ids.filterNot(have.contains).map(result)
        if missing.nonEmpty then
          next match
            // 次の発言の先頭に結果を足す（結果は利用者の発言の最初に置く）
            case Some(n) =>
              out += n.mapObject(_.add("content", Json.fromValues(missing ++ blocks(n).filter(_.hcursor.get[String]("type").contains("tool_result")) ++
                blocks(n).filterNot(_.hcursor.get[String]("type").contains("tool_result")) ++
                n.hcursor.get[String]("content").toOption.map(tx => Json.obj("type" -> "text".asJson, "text" -> tx.asJson)).toList)))
              i += 1
            case None => out += Json.obj("role" -> "user".asJson, "content" -> Json.fromValues(missing))
      i += 1
    Json.fromValues(out.result())

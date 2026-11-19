package skakapo.agent

import cats.effect.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.{Accept, Authorization, `Content-Type`}
import org.typelevel.ci.CIString
import skakapo.shared.ToolInfo

import scala.concurrent.duration.*

/** MCP のツール（Claude に渡すスキーマ付き）. */
final case class McpTool(info: ToolInfo, schema: Json)

/** ツールの結果（MCP の content の文字を繋げたもの）. */
final case class McpResult(text: String, isError: Boolean)

/**
 * 各サービスの MCP（Streamable HTTP の JSON-RPC）を、利用者のトークンで呼ぶクライアント.
 * 応答が SSE（text/event-stream）で返ってきても、最初の data の JSON を読む.
 */
final class Mcp(http: Client[IO]) {
  private val Version = "2025-06-18"

  private def rpc(url: String,
                  token: String,
                  session: Option[String],
                  method: String,
                  params: Json,
                  id: Option[Int]): IO[(Json, Option[String])] = {
    val body = Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "method" -> method.asJson,
      "params" -> params
    ).deepMerge(id.fold(Json.obj())(i => Json.obj("id" -> i.asJson)))
    val req = Request[IO](Method.POST, Uri.unsafeFromString(url))
      .withEntity(body.noSpaces)(using EntityEncoder.stringEncoder)
      .putHeaders(
        `Content-Type`(MediaType.application.json),
        Accept(MediaType.application.json, MediaType.unsafeParse("text/event-stream")),
        Authorization(Credentials.Token(AuthScheme.Bearer, token)),
        Header.Raw(CIString("MCP-Protocol-Version"), Version)
      )
      .putHeaders(session.map(s => Header.Raw(CIString("Mcp-Session-Id"), s)).toList.map(h => h: Header.ToRaw)*)
    http.run(req).use { r =>
      // 大きすぎる応答でメモリを使い切らないよう、10MB まで
      HttpBody.read(r, 10L * 1024 * 1024).map { s =>
        val sid = r.headers.get(CIString("Mcp-Session-Id")).map(_.head.value)
        if r.status == Status.Unauthorized || r.status == Status.Forbidden then
          throw McpError(s"${r.status.code}: トークンが無効か、権限がありません（連携し直してください）", auth = true)
        else if r.status == Status.Accepted || s.trim.isEmpty then (Json.Null, sid)
        else if !r.status.isSuccess then
          throw McpError(s"${r.status.code} ${r.status.reason}: ${s.take(300)}", status = r.status.code)
        else {
          // SSE なら最初の data: の行
          val json = if s.startsWith("event:") || s.startsWith("data:") then
            s.linesIterator.find(_.startsWith("data:")).map(_.drop(5).trim).getOrElse("")
          else s
          val j = parse(json).getOrElse(throw McpError(s"応答を読めません: ${json.take(200)}"))
          j.hcursor.downField("error").focus.filterNot(_.isNull) match {
            case Some(e) => throw McpError(e.hcursor.get[String]("message").getOrElse(e.noSpaces), rpc = true)
            case None => (j.hcursor.downField("result").focus.getOrElse(Json.Null), sid)
          }
        }
      }
    }.timeout(90.seconds)
  }

  /** (URL, トークン) ごとの MCP のセッション（initialize をツールを呼ぶたびにしない）. */
  private val sessions = Ref.unsafe[IO, Map[(String, String), Option[String]]](Map.empty)

  /**
   * セッションを使い回してから呼ぶ. セッションの期限切れ（404・session を含む 400）のときだけ、initialize し直して1回だけ送り直す
   * （時間切れ・5xx では送り直さない. 書き込みのツールが二重に実行されるため）.
   */
  private def withSession[A](url: String, token: String)(f: Option[String] => IO[A]): IO[A] =
    sessions.get.map(_.get(url -> token)).flatMap {
      case Some(sid) => f(sid).handleErrorWith {
          case e: McpError if e.sessionExpired => sessions.update(_ - (url -> token)) *> fresh(url, token).flatMap(f)
          // ツール側のエラー（JSON-RPC の error）ならセッションは生きているので、捨てない
          case e: McpError if e.rpc => IO.raiseError(e)
          case e => sessions.update(_ - (url -> token)) *> IO.raiseError(e)
        }
      case None => fresh(url, token).flatMap(f)
    }

  private def fresh(url: String, token: String): IO[Option[String]] =
    open(url, token).flatTap(sid =>
      sessions.update(m => (if m.size > 1000 then Map.empty else m) + ((url -> token) -> sid))
    )

  /** initialize（と initialized の通知）をして、セッション ID（あれば）を返す. */
  private def open(url: String, token: String): IO[Option[String]] =
    rpc(
      url,
      token,
      None,
      "initialize",
      Json.obj(
        "protocolVersion" -> Version.asJson,
        "capabilities" -> Json.obj(),
        "clientInfo" -> Json.obj("name" -> "skakapo".asJson, "version" -> "0.1.0".asJson)
      ),
      Some(1)
    ).flatMap {
      (_, sid) =>
        rpc(url, token, sid, "notifications/initialized", Json.obj(), None).attempt.as(sid)
    }

  def listTools(url: String, token: String): IO[List[McpTool]] =
    open(url, token).flatMap { sid =>
      def page(cursor: Option[String], acc: List[McpTool], n: Int): IO[List[McpTool]] =
        rpc(
          url,
          token,
          sid,
          "tools/list",
          cursor.fold(Json.obj())(c => Json.obj("cursor" -> c.asJson)),
          Some(2 + n)
        ).flatMap {
          (res, _) =>
            {
              val c = res.hcursor
              val tools = c.downField("tools").as[List[Json]].getOrElse(Nil).flatMap { t =>
                val tc = t.hcursor
                tc.get[String]("name").toOption.map(name =>
                  McpTool(
                    ToolInfo(
                      name,
                      tc.get[String]("description").getOrElse(""),
                      tc.downField("annotations").get[Boolean]("readOnlyHint").toOption
                    ),
                    tc.downField("inputSchema").focus.getOrElse(Json.obj("type" -> "object".asJson))
                  )
                )
              }
              c.get[String]("nextCursor").toOption.filter(_.nonEmpty) match {
                case Some(next) if n < 20 => page(Some(next), acc ++ tools, n + 1)
                case _ => IO.pure(acc ++ tools)
              }
            }
        }
      page(None, Nil, 0)
    }

  def call(url: String, token: String, tool: String, args: Json): IO[McpResult] =
    withSession(url, token)(sid =>
      rpc(url, token, sid, "tools/call", Json.obj("name" -> tool.asJson, "arguments" -> args), Some(10))
    ).map {
      (res, _) =>
        {
          val c = res.hcursor
          val text = c.downField("content").as[List[Json]].getOrElse(Nil).map { b =>
            b.hcursor.get[String]("text").getOrElse(b.noSpaces)
          }.mkString("\n")
          val structured =
            if text.isEmpty then c.downField("structuredContent").focus.map(_.noSpaces).getOrElse("") else ""
          McpResult(if text.nonEmpty then text else structured, c.get[Boolean]("isError").getOrElse(false))
        }
    }
}

final case class McpError(message: String, auth: Boolean = false, status: Int = 0, rpc: Boolean = false)
  extends Exception(message) {
  def sessionExpired: Boolean = status == 404 || (status == 400 && message.toLowerCase.contains("session"))
}

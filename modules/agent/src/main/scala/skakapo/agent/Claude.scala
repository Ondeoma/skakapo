package skakapo.agent

import cats.effect.*
import com.anthropic.client.AnthropicClient
import com.anthropic.client.okhttp.AnthropicOkHttpClient
import com.anthropic.core.JsonValue
import com.anthropic.helpers.MessageAccumulator
import com.anthropic.models.messages.*

import scala.jdk.CollectionConverters.*

/** 1回の応答. */
final case class Turn(message: Message,
                      inputTokens: Long,
                      outputTokens: Long,
                      cacheReadTokens: Long,
                      cacheWriteTokens: Long = 0) {
  def stop: Option[StopReason] = Option(message.stopReason().orElse(null))
}

/** モデルとのやりとり（テストでは決まった応答を返すものに差し替える）. */
trait Llm {
  def turn(model: String,
           maxTokens: Int,
           effort: String,
           fallbacks: Boolean,
           system: List[TextBlockParam],
           tools: List[ToolUnion],
           messages: List[MessageParam]): IO[Turn]
}

/** Claude API の呼び出し（Anthropic の Java SDK）. ANTHROPIC_API_KEY（または SDK が読む資格情報）を使う. */
final class Claude(client: AnthropicClient) extends Llm {

  /**
   * 1ターンを送り、応答を全部受け取って返す. 長い応答（ファイルの書き換えなど）で HTTP がタイムアウトしないよう、ストリーミングで受けて組み立てる.
   * fallbacks は、安全のための分類で断られたときに、サーバー側で別のモデルに任せる（server-side-fallback のベータ）.
   */
  def turn(model: String,
           maxTokens: Int,
           effort: String,
           fallbacks: Boolean,
           system: List[TextBlockParam],
           tools: List[ToolUnion],
           messages: List[MessageParam]): IO[Turn] = IO.blocking {
    val b = MessageCreateParams.builder()
      .model(model)
      .maxTokens(maxTokens.toLong)
      .systemOfTextBlockParams(system.asJava)
      .tools(tools.asJava)
      .messages(messages.asJava)
      .outputConfig(OutputConfig.builder().effort(OutputConfig.Effort.of(effort)).build())
    // 会話の途中で前提（管理者・利用者の前提、連携したサービスのツール）が変わると、それまでの thinking のブロックは使えなくなる.
    // 合わなくなったブロックは捨てて続ける（失敗にしない）
    b.putAdditionalBodyProperty(
      "thinking",
      JsonValue.from(java.util.Map.of(
        "type",
        "adaptive",
        "block_binding",
        java.util.Map.of("prefix_mismatch_behavior", "drop_block")
      ))
    )
    // Haiku にはサーバー側の代わりのモデルがない
    val useFallbacks = fallbacks && skakapo.shared.Models.fallbacks(model)
    b.putAdditionalHeader(
      "anthropic-beta",
      (List("thinking-binding-controls-2026-08-01") ++
        Option.when(useFallbacks)("server-side-fallback-2026-07-01")).mkString(",")
    )
    if useFallbacks then b.putAdditionalBodyProperty("fallbacks", JsonValue.from("default"))
    // 会話の部分もキャッシュする（最後のブロックまで. 毎ターン会話全体を、キャッシュなしの入力として払わない）
    b.cacheControl(CacheControlEphemeral.builder().build())
    val acc = MessageAccumulator.create()
    val stream = client.messages().createStreaming(b.build())
    try stream.stream().forEach(e => acc.accumulate(e))
    finally stream.close()
    val m = acc.message()
    val u = m.usage()
    Turn(
      m,
      u.inputTokens(),
      u.outputTokens(),
      u.cacheReadInputTokens().orElse(0L),
      u.cacheCreationInputTokens().orElse(0L)
    )
  }
}

object Claude {

  /** API キーがなければ None（エージェントは動かせないが、画面と設定は使える）. */
  def fromEnv: Option[Claude] =
    sys.env.get("ANTHROPIC_API_KEY").map(_.trim).filter(_.nonEmpty).map(k =>
      new Claude(AnthropicOkHttpClient.builder().apiKey(k).build())
    )

  /** キャッシュする文（システムプロンプトの変わらない部分）. 1時間残す. */
  def cached(text: String): TextBlockParam =
    TextBlockParam.builder().text(
      text
    ).cacheControl(CacheControlEphemeral.builder().ttl(CacheControlEphemeral.Ttl.TTL_1H).build()).build()

  def text(t: String): TextBlockParam = TextBlockParam.builder().text(t).build()
}

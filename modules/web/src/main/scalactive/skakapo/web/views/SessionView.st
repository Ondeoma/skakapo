<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*
import org.scalajs.dom

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 会話: エージェントの発言・ツールの呼び出しと結果を流し、返事を送る. 新しい出来事は長いポーリングで受け取る（どのノードに繋がってもよい）
case class Props(sessionId: UUID)

case class State(session: RV[Option[SessionDto]] = RV(None), events: RV[List[EventDto]] = RV(Nil), run: RV[Option[RunDto]] = RV(None),
                 usage: RV[Option[UsageDto]] = RV(None),
                 text: RV[String] = RV(""), attach: Attach = Attach(), sending: RV[Boolean] = RV(false), alive: RV[Boolean] = RV(true), last: RV[Long] = RV(0L),
                 editing: RV[Boolean] = RV(false), titleInput: RV[String] = RV(""))

def initState() = State()

def mounted(s: State, props: Props): Unit = {
  pageTitle("会話")
  Store.report(Api.session(props.sessionId)) { d =>
    apply(s, d)
    scrollDown()
    poll(s, props)
  }
}

def unmounted(s: State): Unit = s.alive := false

def apply(s: State, d: EventsDto): Unit = {
  if (!s.session.v.exists(_.title == d.session.title)) pageTitle(d.session.title)
  s.session := Some(d.session)
  s.run := d.run
  // 使用量は、新しい出来事があったときだけ返る（なければ前の値のまま）
  d.usage.foreach(u => s.usage := Some(u))
  if (d.events.nonEmpty) {
    s.events := (s.events.v ++ d.events.filter(_.id > s.last.v)).takeRight(2000)
    s.last := d.events.map(_.id).max.max(s.last.v)
  }
}

/** 新しい出来事を待つ（出たら続けて待つ. 失敗したら少し待ってやり直す）. */
def poll(s: State, props: Props): Unit = if (s.alive.v) {
  Api.events(props.sessionId, s.last.v, 20).onComplete {
    case scala.util.Success(d) =>
      val grew = d.events.nonEmpty
      apply(s, d)
      if (grew) scrollDown()
      if (d.events.exists(e => e.kind == "status" && e.data.hcursor.get[Boolean]("done").getOrElse(false))) Store.refresh()
      poll(s, props)
    case scala.util.Failure(_) => dom.window.setTimeout(() => poll(s, props), 3000)
  }
}

def scrollDown(): Unit = dom.window.setTimeout(() => dom.window.scrollTo(0, dom.document.body.scrollHeight.toInt), 50)

def busy(s: State) = s.session.mapC(_.exists(x => SessionStatus.busy(x.status)))
def statusText(s: State) = s.session.mapC(_.map(x => SessionStatus.label(x.status)).getOrElse(""))
def statusCls(s: State) = s.session.mapC(_.map(x => Fmt.statusClass(x.status)).getOrElse("st"))
def title(s: State) = s.session.mapC(_.map(_.title).getOrElse(""))
def links(s: State) = s.session.mapC(_.map(_.links).getOrElse(Nil))
def details = Store.showDetails

/** 使用量（この依頼のすべての実行の合計）: 費用と、トークン数の内訳. */
def usageCost(s: State) = s.usage.mapC(_.fold("")(u => Usd.fmt(u.costUsd)))
def usageRows(s: State) = s.usage.mapC(_.toList.flatMap(u => List(
  "実行" -> s"${u.runs} 回・${u.turns} ターン",
  "入力" -> Fmt.tokens(u.inputTokens),
  "キャッシュ" -> s"読み ${Fmt.tokens(u.cacheReadTokens)}・書き ${Fmt.tokens(u.cacheWriteTokens)}",
  "出力" -> Fmt.tokens(u.outputTokens))))
def runInfo(s: State) = s.run.mapC(_.map(r =>
  s"${r.node.map(n => s"ノード $n・").getOrElse("")}${r.turns} ターン・入力 ${Fmt.tokens(r.inputTokens)}（キャッシュ ${Fmt.tokens(r.cacheReadTokens)}）・出力 ${Fmt.tokens(r.outputTokens)}").getOrElse(""))

def send(s: State, props: Props): Unit = {
  val t = s.text.v.trim
  if ((t.nonEmpty || s.attach.atts.v.nonEmpty) && !s.sending.v && !s.attach.busy) {
    s.sending := true
    Api.say(props.sessionId, t, s.attach.ids).onComplete { r =>
      s.sending := false
      r.fold(Store.error, _ => { s.text := ""; s.attach.clear(); s.session := s.session.v.map(_.copy(status = SessionStatus.Queued)) })
    }
  }
}

/** Enter で送る. Shift+Enter は改行. 日本語の変換を確定する Enter（変換中. Safari は keyCode 229）では送らない. */
def key(s: State, props: Props, e: dom.Event): Unit = {
  val k = e.asInstanceOf[dom.KeyboardEvent]
  val composing = k.asInstanceOf[scala.scalajs.js.Dynamic].isComposing.asInstanceOf[scala.scalajs.js.UndefOr[Boolean]].getOrElse(false) || k.keyCode == 229
  if (k.key == "Enter" && !k.shiftKey && !composing) { e.preventDefault(); send(s, props) }
}

// 題名を変える（変えたら、エージェントはもう題名を決めない）
def editTitle(s: State): Unit = { s.titleInput := s.session.v.map(_.title).getOrElse(""); s.editing := true }
def saveTitle(s: State, props: Props): Unit = {
  val t = s.titleInput.v.trim
  if (t.nonEmpty) Store.report(Api.renameSession(props.sessionId, t)) { _ =>
    s.session := s.session.v.map(_.copy(title = t, titleAuto = false)); pageTitle(t); s.editing := false; Store.refresh()
  }
}
def titleAuto(s: State) = s.session.mapC(_.exists(_.titleAuto))

def stop(s: State, props: Props): Unit = Store.report(Api.cancel(props.sessionId))(_ => Store.toast("止めるよう頼みました"))

def archive(s: State, props: Props): Unit = SessionActions.archive(props.sessionId, !s.session.v.exists(_.archived))(Store.go("/"))

def remove(s: State, props: Props): Unit = SessionActions.remove(props.sessionId, s.session.v.fold("この依頼")(_.title))(Store.go("/"))

// ---------------------------------------------------------------- 出来事の表示

def text(e: EventDto): String = e.data.hcursor.get[String]("text").getOrElse("")
def html(e: EventDto): String = Markdown.toHtml(text(e))
def toolName(e: EventDto): String = e.data.hcursor.get[String]("name").getOrElse("")
def isError(e: EventDto): Boolean = e.data.hcursor.get[Boolean]("isError").getOrElse(false)

/** ツールの名前を人に分かるように（skea__create_task → skea: create_task）. */
def toolLabel(e: EventDto): String = {
  val n = toolName(e)
  def arg(k: String) = e.data.hcursor.downField("input").get[String](k).getOrElse("")
  ToolNames.split(n) match {
    case Some((svc, tool)) => s"$svc: $tool"
    case None => n match {
      case "str_replace_based_edit_tool" => s"ファイル: ${arg("command")} ${arg("path")}"
      case "checkout_repository"         => s"リポジトリを取り出す: ${arg("repository")}"
      case other                         => other
    }
  }
}
def input(e: EventDto): String = e.data.hcursor.downField("input").focus.map(_.spaces2).getOrElse("")
def resultClass(e: EventDto): String = if (isError(e)) "tool res ng" else "tool res"
def attachments(e: EventDto): List[AttachmentDto] = e.data.hcursor.get[List[AttachmentDto]]("attachments").getOrElse(Nil)
</script>

<template>
  <div class="page wide chat">
    <div class="main-col">
      <p class="small"><RouterLink to="/">← 依頼</RouterLink></p>
      <div class="row head">
        <form v-if="s.editing" class="row grow title-edit" @submit.prevent="saveTitle(s, props)">
          <input type="text" class="grow" v-model="s.titleInput" maxlength="100">
          <button type="submit" class="primary small">保存</button>
          <button type="button" class="ghost small" @click="s.editing := false">やめる</button>
        </form>
        <h1 v-else class="grow title" @click="editTitle(s)" title="押すと題名を変えられます">{{ title(s) }} <span v-if="titleAuto(s)" class="small muted">（自動）</span><span class="pen">✎</span></h1>
        <span :class="statusCls(s)">{{ statusText(s) }}</span>
      </div>
      <div class="events">
        <template v-for="e in s.events" :key="e.id">
          <div v-if='e.kind == "user"' class="msg user"><div class="bubble">
            <div v-if="attachments(e).nonEmpty" class="thumbs">
              <template v-for="a in attachments(e)">
                <a v-if="a.isImage" :href="AttachmentUrl(a)" target="_blank" rel="noopener"><img :src="AttachmentUrl(a)" :alt="a.name"></a>
                <a v-else class="file" :href="AttachmentUrl(a)" :download="a.name" :title="a.name">📄 <span class="nm">{{ a.name }}</span> <span class="small muted">{{ AttachmentRule.size(a.size) }}</span></a>
              </template>
            </div>
            <div class="pre">{{ text(e) }}</div>
          </div></div>
          <div v-else-if='e.kind == "assistant"' class="msg bot"><div class="bubble md" v-html="html(e)"></div></div>
          <details v-else-if='e.kind == "tool_call" && details.v' class="tool call">
            <summary>🔧 {{ toolLabel(e) }}</summary>
            <pre class="code">{{ input(e) }}</pre>
          </details>
          <details v-else-if='e.kind == "tool_result" && details.v' :class="resultClass(e)">
            <summary>{{ if (isError(e)) "⚠️ 失敗" else "↳ 結果" }}<span class="muted small"> {{ text(e).take(80) }}</span></summary>
            <pre class="code">{{ text(e) }}</pre>
          </details>
          <div v-else-if='e.kind == "error"' class="note error-note">⚠️ {{ text(e) }}</div>
          <div v-else-if='e.kind == "note" || details.v' class="note">{{ text(e) }} <span class="muted small">{{ Fmt.time(e.at) }}</span></div>
        </template>
        <div v-if="busy(s)" class="note working"><span class="dot"></span> エージェントが作業しています…</div>
      </div>
      <form class="composer panel" @submit.prevent="send(s, props)" @paste="s.attach.paste(event)" @dragover.prevent="()" @drop="s.attach.drop(event)">
        <textarea v-model="s.text" rows="3" placeholder="返事・追加の指示（Enter で送る・Shift+Enter で改行）" @keydown="key(s, props, event)" :disabled="busy(s).v"></textarea>
        <AttachList v-if="!busy(s).v" :attach="s.attach" :compact="true" />
        <div class="row">
          <button v-if="busy(s)" type="button" class="danger" @click="stop(s, props)">止める</button>
          <span class="grow"></span>
          <button type="submit" class="primary" :disabled="busy(s).v || s.sending.v || s.attach.uploading.v > 0">送る</button>
        </div>
      </form>
    </div>
    <aside class="side">
      <div class="panel">
        <h3>作ったもの</h3>
        <p v-if="links(s).mapC(_.isEmpty)" class="small muted">まだありません</p>
        <ul class="links">
          <li v-for="l in links(s)"><a :href="l.url" target="_blank" rel="noopener noreferrer">{{ l.label }}</a></li>
        </ul>
      </div>
      <div v-if="s.usage.mapC(_.isDefined)" class="panel usage">
        <div class="row ut"><span class="small muted grow">使用量</span><span class="cost">{{ usageCost(s) }}</span></div>
        <dl class="small"><template v-for="r in usageRows(s)"><dt class="muted">{{ r._1 }}</dt><dd>{{ r._2 }}</dd></template></dl>
        <p class="small muted note">この依頼のすべての実行の合計（Claude API の費用を、トークン数とモデルの単価から計算したもの）</p>
      </div>
      <div class="panel small muted">{{ runInfo(s) }}</div>
      <div class="panel">
        <button class="ghost small" @click="archive(s, props)">{{ if (s.session.v.exists(_.archived)) "アーカイブを解除" else "アーカイブ" }}</button>
        <button class="ghost small danger" @click="remove(s, props)">削除</button>
      </div>
    </aside>
  </div>
</template>

<style scoped>
.chat { display: grid; grid-template-columns: minmax(0, 1fr) 280px; gap: 1.2rem; align-items: start; }
@media (max-width: 900px) { .chat { grid-template-columns: 1fr; } }
.head { align-items: center; gap: .6rem; }
.head h1 { margin: .2rem 0; font-size: 1.3rem; }
.events { display: flex; flex-direction: column; gap: .45rem; margin: .8rem 0 1rem; }
.msg { display: flex; }
.msg.user { justify-content: flex-end; }
.bubble { max-width: 85%; border-radius: 12px; padding: .6rem .9rem; line-height: 1.7; overflow-wrap: anywhere; }
.user .bubble { background: var(--accent-weak); border: 1px solid var(--accent); }
.bot .bubble { background: var(--card); border: 1px solid var(--line); }
.pre { white-space: pre-wrap; }
.title { cursor: pointer; }
.title .pen { font-size: .8rem; color: var(--muted); margin-left: .4rem; opacity: 0; }
.title:hover .pen { opacity: 1; }
.title-edit { gap: .4rem; align-items: center; }
.thumbs { display: flex; flex-wrap: wrap; gap: .4rem; margin-bottom: .4rem; }
.thumbs img { width: 120px; height: 90px; object-fit: cover; border-radius: 6px; border: 1px solid var(--line); display: block; }
.thumbs .file { display: inline-flex; align-items: center; gap: .3rem; max-width: 100%; padding: .3rem .6rem; border: 1px solid var(--line); border-radius: 6px; background: var(--card); color: var(--fg); text-decoration: none; }
.thumbs .file .nm { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 260px; }
.tool { font-size: .85rem; border-left: 3px solid var(--line); padding: .15rem .6rem; margin-left: .4rem; }
.tool summary { cursor: pointer; color: var(--muted); }
.tool.res.ng { border-left-color: var(--danger); }
.tool .code { max-height: 360px; overflow: auto; white-space: pre-wrap; font-size: .8rem; background: var(--hover); padding: .5rem; border-radius: 6px; }
.note { text-align: center; font-size: .8rem; color: var(--muted); }
.error-note { color: var(--danger); }
.working .dot { display: inline-block; width: .55rem; height: .55rem; border-radius: 50%; background: var(--accent); animation: blink 1s infinite; }
@keyframes blink { 50% { opacity: .2; } }
.composer { position: sticky; bottom: .6rem; display: flex; flex-direction: column; gap: .5rem; box-shadow: var(--shadow); }
.composer textarea { width: 100%; }
.side { position: sticky; top: calc(var(--header) + 1rem); display: flex; flex-direction: column; gap: .6rem; }
.side h3 { margin: 0 0 .4rem; font-size: .95rem; }
.links { margin: 0; padding-left: 1.1rem; }
.links li { margin: .2rem 0; overflow-wrap: anywhere; }
.usage .ut { align-items: baseline; }
.usage .cost { font-family: var(--mono); font-size: 1.35rem; font-weight: 700; background: var(--grad); -webkit-background-clip: text; background-clip: text; color: transparent; }
.usage dl { display: grid; grid-template-columns: auto 1fr; gap: .15rem .7rem; margin: .4rem 0 .2rem; }
.usage dd { margin: 0; font-family: var(--mono); text-align: right; }
.usage .note { margin: .3rem 0 0; font-size: .74rem; }
</style>

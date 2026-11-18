<script lang="scala">
import skakapo.web.*
import skakapo.shared.*
import org.scalajs.dom

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 新しい依頼: 種類（不具合の修正・機能の追加・調査・その他）を選び、その種類のフォームに入れて始める
case class State(types: RV[List[RequestTypeDto]] = RV(Nil), chosen: RV[Option[RequestTypeDto]] = RV(None),
                 values: RV[Map[String, String]] = RV(Map.empty), services: RV[List[ConnectionDto]] = RV(Nil), repos: RV[List[RepositoryDto]] = RV(Nil),
                 sending: RV[Boolean] = RV(false), error: RV[String] = RV(""),
                 atts: RV[List[AttachmentDto]] = RV(Nil), uploading: RV[Int] = RV(0))

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("新しい依頼")
  Store.report(Api.requestTypes()) { ts =>
    s.types := ts
    Query.get("type").flatMap(k => ts.find(_.key == k)).foreach(choose(s, _))
  }
  Store.report(Api.connections())(cs => s.services := cs)
  Store.report(Api.repositories())(rs => s.repos := rs)
}

def choose(s: State, t: RequestTypeDto): Unit = { s.chosen := Some(t); s.values := Map.empty; s.error := "" }

def set(s: State, name: String, v: String): Unit = s.values := s.values.v.updated(name, v)
def value(s: State, name: String) = s.values.mapC(_.getOrElse(name, ""))

def fields(s: State) = s.chosen.mapC(_.map(_.fields).getOrElse(Nil))
def label(f: FormField): String = f.label + (if (f.required) "" else "（任意）")

/** サービスの選択肢（連携していないものも出す. 連携していなければ一言添える）. */
def serviceOptions(s: State) = s.services.mapC(_.map(c => c.service.key -> (c.service.name + (if (c.connected) "" else "（未連携）"))))
def notConnected(s: State, name: String) = computed {
  val v = s.values.v.getOrElse(name, "")
  s.services.v.find(_.service.key == v).filter(c => !c.connected && c.service.mcpUrl.nonEmpty).map(_.service.name)
}

// ---------------------------------------------------------------- 画像の添付（選ぶ・貼り付ける・落とす）

def upload(s: State, files: List[dom.File]): Unit = files.filter(_.`type`.startsWith("image/")).foreach { f =>
  if (s.atts.v.size + s.uploading.v >= AttachmentRule.MaxCount) Store.toast(s"画像は${AttachmentRule.MaxCount}枚までです", error = true)
  else if (!AttachmentRule.Types.contains(f.`type`)) Store.toast("PNG・JPEG・GIF・WebP の画像を選んでください", error = true)
  else if (f.size > AttachmentRule.MaxBytes) Store.toast("画像は5MBまでにしてください", error = true)
  else {
    s.uploading := s.uploading.v + 1
    Api.uploadAttachment(f, if (f.name.nonEmpty) f.name else "image").onComplete { r =>
      s.uploading := s.uploading.v - 1
      r.fold(Store.error, a => s.atts := s.atts.v :+ a)
    }
  }
}

def pick(s: State, e: dom.Event): Unit = {
  val input = e.target.asInstanceOf[dom.HTMLInputElement]
  upload(s, (0 until input.files.length).map(input.files(_)).toList)
  input.value = ""
}

def paste(s: State, e: dom.Event): Unit = {
  val items = e.asInstanceOf[dom.ClipboardEvent].clipboardData.items
  val files = (0 until items.length).map(items(_)).filter(_.kind == "file").flatMap(i => Option(i.getAsFile())).toList
  if (files.nonEmpty) { e.preventDefault(); upload(s, files) }
}

def drop(s: State, e: dom.Event): Unit = {
  e.preventDefault()
  val fs = e.asInstanceOf[dom.DragEvent].dataTransfer.files
  upload(s, (0 until fs.length).map(fs(_)).toList)
}

def blocked = Store.credit.mapC(_.flatMap(_.blocked).getOrElse(""))
def removeAtt(s: State, a: AttachmentDto): Unit = s.atts := s.atts.v.filterNot(_.id == a.id)

def send(s: State): Unit = s.chosen.v.foreach { t =>
  FormCheck.values(t.fields, s.values.v) match {
    case Left(e) => s.error := e
    case Right(vs) =>
      s.sending := true
      s.error := ""
      Api.start(StartReq(t.key, vs, s.atts.v.map(_.id))).onComplete { r =>
        s.sending := false
        r.fold(e => s.error := e.getMessage, x => { Store.refresh(); Store.go(s"/sessions/${x.id}") })
      }
  }
}
</script>

<template>
  <div class="page narrow">
    <p class="small"><RouterLink to="/">← 依頼</RouterLink></p>
    <h1>新しい依頼</h1>
    <template v-if="s.chosen.mapC(_.isEmpty)">
      <p class="muted">どんな依頼ですか？</p>
      <div class="types">
        <button v-for="t in s.types" class="type card" @click="choose(s, t)">
          <span class="icon">{{ t.icon }}</span>
          <span class="grow"><b>{{ t.label }}</b><br><span class="small muted">{{ t.description }}</span></span>
        </button>
      </div>
    </template>
    <form v-else class="panel" @submit.prevent="send(s)" @paste="paste(s, event)" @dragover.prevent="()" @drop="drop(s, event)">
      <div class="row chosen">
        <span class="icon">{{ s.chosen.v.map(_.icon).getOrElse("") }}</span>
        <b class="grow">{{ s.chosen.v.map(_.label).getOrElse("") }}</b>
        <button type="button" class="ghost small" @click="s.chosen := None">種類を変える</button>
      </div>
      <template v-for="f in fields(s)">
        <label class="field">
          <span>{{ label(f) }}</span>
          <textarea v-if='f.kind == "textarea"' :value="value(s, f.name)" @input="set(s, f.name, Dom.value(event))" rows="5" :placeholder="f.placeholder"></textarea>
          <select v-else-if='f.kind == "select"' :value="value(s, f.name)" @change="set(s, f.name, Dom.value(event))">
            <option value="">（選ぶ）</option>
            <option v-for="o in f.options" :value="o">{{ o }}</option>
          </select>
          <select v-else-if='f.kind == "service"' :value="value(s, f.name)" @change="set(s, f.name, Dom.value(event))">
            <option value="">（選ぶ）</option>
            <option v-for="o in serviceOptions(s)" :value="o._1">{{ o._2 }}</option>
          </select>
          <select v-else-if='f.kind == "repository"' :value="value(s, f.name)" @change="set(s, f.name, Dom.value(event))">
            <option value="">（選ぶ）</option>
            <option v-for="r in s.repos" :value="r.name">{{ r.name }}{{ if (r.description.nonEmpty) s"（${r.description.take(40)}）" else "" }}</option>
          </select>
          <input v-else type="text" :value="value(s, f.name)" @input="set(s, f.name, Dom.value(event))" :placeholder="f.placeholder" maxlength="1000">
          <span v-if="f.help.nonEmpty" class="small muted">{{ f.help }}</span>
        </label>
        <p v-if='f.kind == "service" && notConnected(s, f.name).v.isDefined' class="small warn-text">
          {{ notConnected(s, f.name).v.getOrElse("") }} とはまだ連携していません。エージェントがこのサービスを読み書きするには、<RouterLink to="/connections">連携</RouterLink>してください。
        </p>
      </template>
      <div class="field">
        <span>画像（任意. 画面のスクリーンショットなど. 貼り付け・ドラッグでも添付できます）</span>
        <div class="atts">
          <div v-for="a in s.atts" :key="a.id.toString" class="att">
            <img :src="AttachmentUrl(a)" :alt="a.name">
            <button type="button" class="ghost small rm" title="外す" @click="removeAtt(s, a)">×</button>
          </div>
          <label class="att add">
            <input type="file" accept="image/png,image/jpeg,image/gif,image/webp" multiple @change="pick(s, event)">
            <span>{{ if (s.uploading.v > 0) "上げています…" else "＋ 画像" }}</span>
          </label>
        </div>
      </div>
      <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      <p v-if="blocked.v.nonEmpty" class="error">{{ blocked }}</p>
      <div class="row actions">
        <span class="grow small muted">送ると、エージェントが作業を始めます。途中で質問されたら、会話の画面で答えてください。</span>
        <button type="submit" class="primary" :disabled="s.sending.v || s.uploading.v > 0 || blocked.v.nonEmpty">{{ if (s.sending.v) "送っています…" else "依頼する" }}</button>
      </div>
    </form>
  </div>
</template>

<style scoped>
.types { display: grid; grid-template-columns: repeat(auto-fill, minmax(260px, 1fr)); gap: .6rem; }
.type { display: flex; gap: .8rem; align-items: flex-start; text-align: left; padding: 1rem; cursor: pointer; background: var(--card); white-space: normal; min-width: 0; }
.type:hover { border-color: var(--accent); }
.icon { font-size: 1.6rem; line-height: 1; }
.chosen { align-items: center; gap: .6rem; margin-bottom: .8rem; }
.actions { margin-top: .8rem; align-items: center; gap: .8rem; }
.warn-text { color: var(--warn); margin-top: -.3rem; }
.atts { display: flex; flex-wrap: wrap; gap: .5rem; }
.att { position: relative; width: 96px; height: 96px; border: 1px solid var(--line); border-radius: 8px; overflow: hidden; background: var(--hover); }
.att img { width: 100%; height: 100%; object-fit: cover; display: block; }
.att .rm { position: absolute; top: 2px; right: 2px; background: var(--surface); padding: 0 .4rem; border-radius: 999px; }
.att.add { display: flex; align-items: center; justify-content: center; cursor: pointer; color: var(--muted); border-style: dashed; font-size: .85rem; }
.att.add input { display: none; }
</style>

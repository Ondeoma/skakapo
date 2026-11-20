<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 新しい依頼: 種類（不具合の修正・機能の追加・調査・その他）を選び、その種類のフォームに入れて始める
case class State(types: RV[List[RequestTypeDto]] = RV(Nil), chosen: RV[Option[RequestTypeDto]] = RV(None),
                 values: RV[Map[String, String]] = RV(Map.empty), services: RV[List[ConnectionDto]] = RV(Nil), repos: RV[List[RepositoryDto]] = RV(Nil),
                 sending: RV[Boolean] = RV(false), error: RV[String] = RV(""),
                 attach: Attach = Attach())

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("新しい依頼")
  Toasts.report(Api.requestTypes()) { ts =>
    s.types := ts
    Router.query.v.get("type").flatMap(k => ts.find(_.key == k)).foreach(choose(s, _))
  }
  Toasts.report(Api.connections())(cs => s.services := cs)
  Toasts.report(Api.repositories())(rs => s.repos := rs)
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

def blocked = Store.credit.mapC(_.flatMap(_.blocked).getOrElse(""))

def send(s: State): Unit = s.chosen.v.foreach { t =>
  FormCheck.values(t.fields, s.values.v) match {
    case Left(e) => s.error := e
    case Right(vs) =>
      s.sending := true
      s.error := ""
      Api.start(StartReq(t.key, vs, s.attach.ids)).onComplete { r =>
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
    <form v-else class="panel" @submit.prevent="send(s)" @paste="s.attach.paste(event)" @dragover.prevent="()" @drop="s.attach.drop(event)">
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
        <span>添付ファイル（任意. 画面のスクリーンショット・ログ・資料など. 種類は問いません. 貼り付け・ドラッグでも添付できます）</span>
        <AttachList :attach="s.attach" />
      </div>
      <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      <p v-if="blocked.v.nonEmpty" class="error">{{ blocked }}</p>
      <div class="row actions">
        <span class="grow small muted">送ると、エージェントが作業を始めます。途中で質問されたら、会話の画面で答えてください。</span>
        <button type="submit" class="primary" :disabled="s.sending.v || s.attach.uploading.v > 0 || blocked.v.nonEmpty">{{ if (s.sending.v) "送っています…" else "依頼する" }}</button>
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
</style>

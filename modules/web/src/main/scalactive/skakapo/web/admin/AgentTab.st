<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// エージェントの設定: 必ず最初に読ませる前提（CLAUDE.md の役割）・モデル・effort・上限・起票先
// 起票先の1行
case class Tr(name: RV[String], service: RV[String], project: RV[String], scope: RV[String], notes: RV[String], isDefault: RV[Boolean])

case class State(model: RV[String] = RV(""), effort: RV[String] = RV("high"), maxTurns: RV[String] = RV("60"), maxTokens: RV[String] = RV("32000"),
                 base: RV[String] = RV(""), fallbacks: RV[Boolean] = RV(true), writes: RV[Boolean] = RV(true),
                 trackers: RV[List[Tr]] = RV(Nil),
                 ready: RV[Boolean] = RV(true), prompt: RV[Option[String]] = RV(None), saving: RV[Boolean] = RV(false))

def initState() = State()

def mounted(s: State): Unit = Store.report(Api.adminSettings())(d => fill(s, d))

def fill(s: State, d: AdminSettingsDto): Unit = {
  val a = d.agent
  s.model := a.model; s.effort := a.effort; s.maxTurns := a.maxTurns.toString; s.maxTokens := a.maxTokens.toString
  s.base := a.baseContext; s.fallbacks := a.fallbacks; s.writes := a.allowWrites; s.ready := d.agentReady
  s.trackers := d.trackers.map(tr)
}

def tr(t: TrackerSettings): Tr = Tr(RV(t.name), RV(t.service), RV(t.project), RV(t.scope), RV(t.notes), RV(t.isDefault))
def addTracker(s: State): Unit = s.trackers := s.trackers.v :+ tr(TrackerSettings(s"起票先${s.trackers.v.size + 1}", isDefault = s.trackers.v.isEmpty))
def removeTracker(s: State, t: Tr): Unit = s.trackers := s.trackers.v.filterNot(_ eq t)
def makeDefault(s: State, t: Tr): Unit = s.trackers.v.foreach(x => x.isDefault := (x eq t))

def save(s: State): Unit = {
  s.saving := true
  val r = AdminSettingsReq(
    AgentSettings(s.model.v.trim, s.effort.v, s.maxTurns.v.toIntOption.getOrElse(60), s.maxTokens.v.toIntOption.getOrElse(32000), s.base.v,
      s.fallbacks.v, s.writes.v),
    s.trackers.v.map(t => TrackerSettings(t.name.v.trim, t.service.v.trim, t.project.v.trim, t.scope.v, t.notes.v, t.isDefault.v)))
  Api.saveAdminSettings(r).onComplete { x =>
    s.saving := false
    x.fold(Store.error, d => { fill(s, d); Store.toast("保存しました"); s.prompt := None })
  }
}

def preview(s: State): Unit = Store.report(Api.adminPrompt())(t => s.prompt := Some(t))
def prompts(s: State) = s.prompt.mapC(_.toList)
val vars: String = "{" + "{tracker.service}}・{" + "{tracker.project}}"
</script>

<template>
  <form class="panel" @submit.prevent="save(s)">
    <p v-if="!s.ready.v" class="error">ANTHROPIC_API_KEY が設定されていないため、エージェントは動きません（サーバーの環境変数に設定してください）。</p>
    <label class="field"><span>前提のコンテキスト（すべての会話で、最初に必ず読ませる. CLAUDE.md の役割）</span>
      <textarea class="code" v-model="s.base" rows="12" placeholder="- 回答は日本語で書く&#10;- 本番のデータは変更しない"></textarea></label>
    <div class="row">
      <label class="field grow"><span>モデル</span><select v-model="s.model"><option v-for="m in Models.all" :value="m._1">{{ m._2 }}</option></select></label>
      <label class="field"><span>考える深さ（effort）</span><select v-model="s.effort"><option v-for="e in Effort.all" :value="e">{{ e }}</option></select></label>
      <label class="field"><span>1回の実行のターンの上限</span><input type="number" v-model="s.maxTurns" min="1" max="300"></label>
      <label class="field"><span>応答の最大トークン</span><input type="number" v-model="s.maxTokens" min="1024" max="128000" step="1024"></label>
    </div>
    <label class="row"><input type="checkbox" v-model="s.writes"> 書き込みを許す（外すと、サービスの参照と、コードの調査だけ. commit・push・プルリクエストもしない）</label>
    <label class="row"><input type="checkbox" v-model="s.fallbacks"> 安全のための判断で断られたとき、サーバー側で別のモデルに任せる（server-side fallback）</label>
    <h3>不具合・依頼の起票先</h3>
    <p class="small muted">エージェントは、依頼の対象に合う起票先を選びます（迷えば既定）。依頼の種類の手順からは、既定の起票先を <code>{{ vars }}</code> で参照します。</p>
    <div v-for="t in s.trackers" class="tracker card">
      <div class="row">
        <label class="field"><span>名前</span><input type="text" v-model="t.name" maxlength="50"></label>
        <label class="field"><span>サービスのキー</span><input type="text" v-model="t.service" placeholder="skea"></label>
        <label class="field grow"><span>プロジェクト</span><input type="text" v-model="t.project" placeholder="SKEA"></label>
        <label class="row def"><input type="radio" name="tracker-default" :checked="t.isDefault.v" @change="makeDefault(s, t)"> 既定</label>
        <button type="button" class="ghost small danger" @click="removeTracker(s, t)">削除</button>
      </div>
      <label class="field"><span>対象（どの依頼をここに起票するか. サービス・内容）</span><input type="text" v-model="t.scope" placeholder="例: 経理システム・請求書まわりの不具合"></label>
      <label class="field"><span>起票の決まり（題名の付け方・ラベル・担当など）</span><textarea v-model="t.notes" rows="3"></textarea></label>
    </div>
    <p><button type="button" class="ghost small" @click="addTracker(s)">＋ 起票先を足す</button></p>
    <div class="row actions">
      <button type="button" class="ghost" @click="preview(s)">エージェントに渡す前提を見る</button>
      <span class="grow"></span>
      <button type="submit" class="primary" :disabled="s.saving.v">保存</button>
    </div>
  </form>
  <Modal v-for="p in prompts(s)" :onClose="_ => s.prompt := None" :xwide="true">
    <h2>エージェントに渡す前提（すべての会話で同じ部分）</h2>
    <pre class="prompt">{{ p }}</pre>
    <div class="actions"><button @click="s.prompt := None">閉じる</button></div>
  </Modal>
</template>

<style scoped>
.actions { margin-top: .8rem; align-items: center; }
h3 { margin: 1.2rem 0 .3rem; font-size: 1rem; }
.tracker { margin-bottom: .6rem; padding: .7rem .9rem; }
.tracker .row { align-items: flex-end; gap: .6rem; flex-wrap: wrap; }
.def { align-items: center; gap: .3rem; margin-bottom: .7rem; white-space: nowrap; }
.prompt { white-space: pre-wrap; max-height: 65vh; overflow: auto; background: var(--hover); padding: .8rem; border-radius: 8px; font-size: .82rem; }
</style>

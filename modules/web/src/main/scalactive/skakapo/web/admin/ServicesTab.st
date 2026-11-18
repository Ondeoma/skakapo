<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*

import java.util.UUID

// 連携するサービス: URL・MCP・OAuth のクライアント（各サービスの OAUTH_CLIENTS に skakapo を登録したもの）・エージェントへの説明と決まり.
// ツールは、だれかが連携したときに読んで覚える（システムとして「何ができるか」を把握する）
case class Edit(id: Option[UUID], key: RV[String], name: RV[String], kind: RV[String], baseUrl: RV[String], mcpUrl: RV[String],
                description: RV[String], notes: RV[String], authorizeUrl: RV[String], tokenUrl: RV[String], clientId: RV[String],
                scopes: RV[String], secret: RV[String], hasSecret: Boolean, manualToken: RV[Boolean], enabled: RV[Boolean], position: RV[String],
                tools: List[ToolInfo], agentAccount: RV[String], agentToken: RV[String], hasAgentToken: Boolean)

case class State(list: RV[List[ServiceDto]] = RV(Nil), edit: RV[Option[Edit]] = RV(None), error: RV[String] = RV(""))

def initState() = State()

def mounted(s: State): Unit = load(s)

def load(s: State): Unit = Store.report(Api.adminServices())(xs => s.list := xs)

def open(s: State, x: Option[ServiceDto]): Unit = {
  val d = x.getOrElse(ServiceDto(new UUID(0L, 0L), "", "", ServiceKind.Sbirds, "", "", "", "", OAuthConfig(), false, true, true, s.list.v.size + 1))
  s.error := ""
  s.edit := Some(Edit(x.map(_.id), RV(d.key), RV(d.name), RV(d.kind), RV(d.baseUrl), RV(d.mcpUrl), RV(d.description), RV(d.notes),
    RV(d.oauth.authorizeUrl), RV(d.oauth.tokenUrl), RV(d.oauth.clientId), RV(d.oauth.scopes), RV(""), d.hasSecret, RV(d.manualToken),
    RV(d.enabled), RV(d.position.toString), d.tools, RV(d.agentAccount), RV(""), d.hasAgentToken))
}

/** sBirds シリーズの既定（URL から MCP と OAuth の入口を作る）. */
def fillDefaults(e: Edit): Unit = {
  val b = e.baseUrl.v.trim.stripSuffix("/")
  if (b.nonEmpty) {
    if (e.kind.v == ServiceKind.Sbirds) {
      e.mcpUrl := b + "/mcp"; e.authorizeUrl := b + "/oauth/authorize"; e.tokenUrl := b + "/oauth/token"
      if (e.scopes.v.isEmpty) e.scopes := "read write"
    } else if (e.kind.v == ServiceKind.Github) {
      e.authorizeUrl := b + "/login/oauth/authorize"; e.tokenUrl := b + "/login/oauth/access_token"; e.mcpUrl := ""
      if (e.scopes.v.isEmpty) e.scopes := "repo"
    }
  }
}

/** 画面の値から保存の要求を作る. secret・agentToken: None は変えない、Some("") は消す. */
def req(e: Edit, secret: Option[String], agentToken: Option[String]): ServiceReq =
  ServiceReq(e.key.v.trim, e.name.v.trim, e.kind.v, e.baseUrl.v.trim, e.mcpUrl.v.trim, e.description.v, e.notes.v,
    OAuthConfig(e.authorizeUrl.v.trim, e.tokenUrl.v.trim, e.clientId.v.trim, e.scopes.v.trim),
    secret, e.manualToken.v, e.enabled.v, e.position.v.toIntOption.getOrElse(0), e.agentAccount.v.trim, agentToken)

def entered(v: RV[String]): Option[String] = Option(v.v.trim).filter(_.nonEmpty)

def save(s: State, e: Edit): Unit =
  Store.saved(Api.saveService(e.id, req(e, entered(e.secret), entered(e.agentToken))), s.error) { s.edit := None; load(s) }

/** 保存した秘密を削除する（secret か agentToken の片方を Some("") にして保存する）. */
private def clear(s: State, e: Edit, message: String, secret: Option[String], agentToken: Option[String]): Unit = Dialog.confirm(message, "削除") {
  e.id.foreach(id => Store.report(Api.saveService(Some(id), req(e, secret, agentToken)))(_ => { s.edit := None; load(s) }))
}
def clearSecret(s: State, e: Edit): Unit = clear(s, e, "OAuth のクライアントのシークレットを削除しますか？（OAuth で連携できなくなります）", Some(""), None)
def clearAgentToken(s: State, e: Edit): Unit =
  clear(s, e, "エージェント専用アカウントのキーを削除しますか？（連携していない利用者の依頼では、このサービスを使えなくなります）", None, Some(""))

def remove(s: State, x: ServiceDto): Unit = Dialog.confirm(s"${x.name} を削除しますか？（利用者の連携も削除されます）", "削除") {
  Store.report(Api.deleteService(x.id))(_ => load(s))
}

def edits(s: State) = s.edit.mapC(_.toList)
def oauthState(x: ServiceDto): String =
  if (x.oauth.usable && x.hasSecret) "OAuth ✓" else if (x.oauth.usable) "シークレットなし" else if (x.manualToken) "トークンのみ" else "—"
def toolsText(x: ServiceDto): String = if (x.tools.isEmpty) "—" else s"${x.tools.size} 個"
def agentText(x: ServiceDto): String = if (!x.hasAgentToken) "—" else if (x.agentAccount.nonEmpty) x.agentAccount else "設定済み"
</script>

<template>
  <div class="panel">
    <div class="row head">
      <p class="grow small muted">各サービスの OAUTH_CLIENTS に、skakapo（redirectUri は skakapo の URL の /api/connections/callback・スコープ read write）を登録して、そのクライアントの ID とシークレットをここに入れます。</p>
      <button class="primary" @click="open(s, None)">＋ サービスを追加</button>
    </div>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>サービス</th><th>種類</th><th>URL</th><th>連携</th><th>専用アカウント</th><th>ツール</th><th>状態</th><th></th></tr></thead>
        <tbody>
          <tr v-for="x in s.list" class="click" @click="open(s, Some(x))">
            <td><b>{{ x.name }}</b> <span class="small muted">{{ x.key }}</span></td>
            <td>{{ ServiceKind.label(x.kind) }}</td>
            <td class="small">{{ x.baseUrl }}</td>
            <td class="small">{{ oauthState(x) }}</td>
            <td class="small">{{ agentText(x) }}</td>
            <td class="small">{{ toolsText(x) }}</td>
            <td><span :class='if (x.enabled) "st st-done" else "st"'>{{ if (x.enabled) "有効" else "無効" }}</span></td>
            <td><button class="ghost small danger" @click.stop="remove(s, x)">削除</button></td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
  <Modal v-for="e in edits(s)" :onClose="_ => s.edit := None" :xwide="true">
    <h2>{{ if (e.id.isDefined) "サービスの編集" else "サービスを追加" }}</h2>
    <form @submit.prevent="save(s, e)">
      <div class="row">
        <label class="field"><span>キー（ツールの名前の頭. 英小文字）</span><input type="text" v-model="e.key" placeholder="skea" required></label>
        <label class="field grow"><span>名前</span><input type="text" v-model="e.name" required></label>
        <label class="field"><span>種類</span><select v-model="e.kind"><option v-for="k in ServiceKind.all" :value="k">{{ ServiceKind.label(k) }}</option></select></label>
        <label class="field"><span>並び順</span><input type="number" v-model="e.position"></label>
      </div>
      <div class="row">
        <label class="field grow"><span>URL</span><input type="url" v-model="e.baseUrl" placeholder="https://skea.example.com"></label>
        <button type="button" class="ghost small fill" @click="fillDefaults(e)">URL から既定の入口を入れる</button>
      </div>
      <label class="field"><span>MCP の URL（空なら MCP を使わない）</span><input type="url" v-model="e.mcpUrl" placeholder="https://skea.example.com/mcp"></label>
      <label class="field"><span>説明（何をするサービスか. エージェントに伝える）</span><textarea v-model="e.description" rows="2"></textarea></label>
      <label class="field"><span>決まり・注意（エージェントに伝える. 例: 起票は SKEA プロジェクトに・本番のデータは変えない）</span><textarea v-model="e.notes" rows="4"></textarea></label>
      <h3>OAuth（利用者がこのサービスで許可して、トークンを発行してもらう）</h3>
      <div class="row">
        <label class="field grow"><span>認可の URL</span><input type="url" v-model="e.authorizeUrl"></label>
        <label class="field grow"><span>トークンの URL</span><input type="url" v-model="e.tokenUrl"></label>
      </div>
      <div class="row">
        <label class="field grow"><span>クライアントの ID</span><input type="text" v-model="e.clientId"></label>
        <label class="field grow"><span>スコープ</span><input type="text" v-model="e.scopes" placeholder="read write"></label>
        <label class="field grow"><span>{{ if (e.hasSecret) "シークレット（変えるときだけ入れる）" else "シークレット" }}</span><input type="password" v-model="e.secret" autocomplete="off"></label>
      </div>
      <p v-if="e.hasSecret" class="small"><a href="#" @click.prevent="clearSecret(s, e)">保存したシークレットを削除</a></p>
      <h3>エージェント専用アカウント（利用者が連携していないときに、エージェントが使う）</h3>
      <p class="small muted">そのサービスの管理画面の「エージェント」で専用アカウントを作り、発行したキーを貼り付けます。
        利用者が自分で連携していればその権限で、していなければこのアカウントの権限で動きます。書き込むときは、依頼者の名前を本文に書き添えさせます。</p>
      <div class="row">
        <label class="field grow"><span>アカウントの名前（表示用）</span><input type="text" v-model="e.agentAccount" placeholder="skakapo エージェント"></label>
        <label class="field grow"><span>{{ if (e.hasAgentToken) "キー（変えるときだけ入れる）" else "キー" }}</span>
          <input type="password" v-model="e.agentToken" autocomplete="new-password"></label>
      </div>
      <p v-if="e.hasAgentToken" class="small"><a href="#" @click.prevent="clearAgentToken(s, e)">保存したキーを削除</a></p>
      <label class="row"><input type="checkbox" v-model="e.manualToken"> 利用者が API トークンを貼り付けて連携してもよい</label>
      <label class="row"><input type="checkbox" v-model="e.enabled"> 有効（エージェントに伝え、連携の画面に出す）</label>
      <template v-if="e.tools.nonEmpty">
        <h3>覚えているツール（{{ e.tools.size }} 個）</h3>
        <ul class="tools small">
          <li v-for="t in e.tools"><code>{{ t.name }}</code> {{ t.description.take(120) }}</li>
        </ul>
      </template>
      <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      <div class="actions"><button type="button" @click="s.edit := None">やめる</button><button type="submit" class="primary">保存</button></div>
    </form>
  </Modal>
</template>

<style scoped>
.fill { align-self: flex-end; margin-bottom: .6rem; }
h3 { margin: 1rem 0 .3rem; font-size: .95rem; }
.tools { max-height: 200px; overflow: auto; padding-left: 1.1rem; }
</style>

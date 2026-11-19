<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*

import java.util.UUID

// リポジトリ: エージェントが取り出して調べ・直し・プルリクエストを出せるもの. どのサービスのコードか・作業の決まり（ビルド・テストの方法など）
case class Edit(id: Option[UUID], name: RV[String], url: RV[String], branch: RV[String], service: RV[String], description: RV[String],
                notes: RV[String], enabled: RV[Boolean], account: RV[String])

// 共有アカウント: 開発者でない利用者の依頼でも、このアカウントで commit・push・プルリクエストを出す. トークンは画面に戻さない
case class AccountEdit(id: Option[UUID], name: RV[String], provider: RV[String], apiUrl: RV[String], username: RV[String], token: RV[String],
                       hasToken: Boolean, authorName: RV[String], authorEmail: RV[String], authKind: RV[String], appId: RV[String], installationId: RV[String])

case class State(list: RV[List[RepositoryDto]] = RV(Nil), services: RV[List[ServiceDto]] = RV(Nil), edit: RV[Option[Edit]] = RV(None),
                 error: RV[String] = RV(""), accounts: RV[List[GitAccountDto]] = RV(Nil), accountEdit: RV[Option[AccountEdit]] = RV(None))

def initState() = State()

def mounted(s: State): Unit = {
  load(s)
  Store.report(Api.adminServices())(xs => s.services := xs)
}

def load(s: State): Unit = {
  Store.report(Api.adminRepositories())(xs => s.list := xs)
  Store.report(Api.gitAccounts())(xs => s.accounts := xs)
}

def open(s: State, x: Option[RepositoryDto]): Unit = {
  s.error := ""
  s.edit := Some(x.fold(Edit(None, RV(""), RV("https://github.com/"), RV("main"), RV(""), RV(""), RV(""), RV(true), RV("")))(r =>
    Edit(Some(r.id), RV(r.name), RV(r.url), RV(r.defaultBranch), RV(r.serviceKey), RV(r.description), RV(r.notes), RV(r.enabled),
      RV(r.accountId.fold("")(_.toString)))))
}

def save(s: State, e: Edit): Unit =
  Store.saved(Api.saveRepository(e.id, RepositoryReq(e.name.v.trim, e.url.v.trim, e.branch.v.trim, e.service.v.trim, e.description.v, e.notes.v, e.enabled.v,
    Option(e.account.v).filter(_.nonEmpty).map(UUID.fromString))), s.error) { s.edit := None; load(s) }

def remove(s: State, x: RepositoryDto): Unit = Dialog.confirm(s"${x.name} を削除しますか？（リポジトリそのものは削除されません）", "削除") {
  Store.report(Api.deleteRepository(x.id))(_ => load(s))
}

def openAccount(s: State, x: Option[GitAccountDto]): Unit = {
  s.error := ""
  s.accountEdit := Some(x.fold(AccountEdit(None, RV(""), RV(GitProvider.Github), RV(""), RV(""), RV(""),
      false, RV(""), RV(""), RV(GitAuthKind.Token), RV(""), RV("")))(a =>
    AccountEdit(Some(a.id), RV(a.name), RV(a.provider), RV(a.apiUrl), RV(a.username), RV(""), a.hasToken, RV(a.authorName), RV(a.authorEmail),
      RV(a.authKind), RV(a.appId), RV(a.installationId))))
}


def saveAccount(s: State, e: AccountEdit): Unit =
  Store.saved(Api.saveGitAccount(e.id, GitAccountReq(e.name.v.trim, e.provider.v, e.apiUrl.v.trim, e.username.v.trim, Option(e.token.v.trim).filter(_.nonEmpty),
      e.authorName.v.trim, e.authorEmail.v.trim, kindOf(e), e.appId.v.trim, e.installationId.v.trim)), s.error) { s.accountEdit := None; load(s) }

/** ホストで選べない方式になっていたら、アクセストークンに戻す（ホストを変えたとき）. */
def kindOf(e: AccountEdit): String = if (GitAuthKind.forProvider(e.provider.v).contains(e.authKind.v)) e.authKind.v else GitAuthKind.Token
def kinds(e: AccountEdit) = e.provider.mapC(GitAuthKind.forProvider)
def isKind(e: AccountEdit, k: String) = e.authKind.mapC(_ => kindOf(e) == k)
def secretLabel(e: AccountEdit): String = (kindOf(e), e.hasToken) match {
  case (GitAuthKind.GithubApp, has)      => if (has) "秘密鍵（PEM. 変えるときだけ貼り付ける）" else "秘密鍵（PEM. App の設定の Private keys で作った .pem の中身）"
  case (GitAuthKind.BitbucketOAuth, has) => if (has) "シークレット（変えるときだけ入れる）" else "シークレット"
  case (_, has)                          => if (has) "トークン（変えるときだけ入れる）" else "トークン（repo の読み書き・プルリクエストの権限）"
}
def authorHint(e: AccountEdit): String = if (kindOf(e) == GitAuthKind.GithubApp) "空なら App のボット（<slug>[bot]）" else "空なら依頼者"
def accountUser(a: GitAccountDto): String =
  if (a.authKind != GitAuthKind.Token) s"${GitAuthKind.label(a.authKind)}（${a.appId}）"
  else if (a.username.isEmpty) GitProvider.defaultUser(a.provider) else a.username

def removeAccount(s: State, x: GitAccountDto): Unit =
  Dialog.confirm(s"${x.name} を削除しますか？（このアカウントを使うリポジトリは、利用者の GitHub の連携に戻ります）", "削除") {
    Store.report(Api.deleteGitAccount(x.id))(_ => load(s))
  }

def accountName(s: State, id: Option[UUID]): String =
  id.fold("利用者の連携")(i => s.accounts.v.find(_.id == i).fold("（削除されたアカウント）")(_.name))

def userHint(p: String): String =
  if (p == GitProvider.Bitbucket) "空ならアクセストークン（x-token-auth）。app password ならユーザー名"
  else if (GitProvider.needsUser(p)) "そのアカウントのユーザー名（必須）"
  else s"空なら ${GitProvider.defaultUser(p)}（トークンだけで認証）"

def accountEdits(s: State) = s.accountEdit.mapC(_.toList)
def edits(s: State) = s.edit.mapC(_.toList)
def serviceKeys(s: State) = s.services.mapC(_.map(_.key))
</script>

<template>
  <div class="panel">
    <div class="row head">
      <p class="grow small muted">sBirds シリーズの外のサービスのリポジトリも登録できます（関係するサービスに自由な名前を書く）。共有アカウントを選んだリポジトリは、開発者でない利用者の依頼でも、そのアカウントで commit・push・プルリクエストを出します（選ばなければ、利用者の GitHub との連携を使います）。</p>
      <button class="primary" @click="open(s, None)">＋ リポジトリを追加</button>
    </div>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>名前</th><th>URL</th><th>既定のブランチ</th><th>サービス</th><th>push の名義</th><th>状態</th><th></th></tr></thead>
        <tbody>
          <tr v-for="x in s.list" class="click" @click="open(s, Some(x))">
            <td><b>{{ x.name }}</b></td>
            <td class="small">{{ x.url }}</td>
            <td>{{ x.defaultBranch }}</td>
            <td>{{ x.serviceKey }}</td>
            <td class="small">{{ accountName(s, x.accountId) }}</td>
            <td><span :class='if (x.enabled) "st st-done" else "st"'>{{ if (x.enabled) "有効" else "無効" }}</span></td>
            <td><button class="ghost small danger" @click.stop="remove(s, x)">削除</button></td>
          </tr>
        </tbody>
      </table>
    </div>
    <p v-if="s.list.mapC(_.isEmpty)" class="empty">リポジトリはまだありません。</p>
  </div>
  <div class="panel">
    <div class="row head">
      <div class="grow">
        <h3>Git の共有アカウント</h3>
        <p class="small muted">GitHub・Bitbucket・Forgejo などに {{ AppNames.self }} 専用のユーザーを作り、そのトークンを登録します。トークンは暗号化して保存し、画面には戻しません。コミットには依頼者を Requested-by として書き添えます。</p>
      </div>
      <button @click="openAccount(s, None)">＋ アカウントを追加</button>
    </div>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>名前</th><th>ホスト</th><th>認証</th><th>コミットの作者</th><th>秘密</th><th></th></tr></thead>
        <tbody>
          <tr v-for="a in s.accounts" class="click" @click="openAccount(s, Some(a))">
            <td><b>{{ a.name }}</b></td>
            <td>{{ GitProvider.label(a.provider) }}</td>
            <td class="small">{{ accountUser(a) }}</td>
            <td class="small">{{ a.authorName }} &lt;{{ a.authorEmail }}&gt;</td>
            <td><span :class='if (a.hasToken) "st st-done" else "st"'>{{ if (a.hasToken) "登録済み" else "なし" }}</span></td>
            <td><button class="ghost small danger" @click.stop="removeAccount(s, a)">削除</button></td>
          </tr>
        </tbody>
      </table>
    </div>
    <p v-if="s.accounts.mapC(_.isEmpty)" class="empty">共有アカウントはまだありません。</p>
  </div>
  <Modal v-for="e in accountEdits(s)" :onClose="_ => s.accountEdit := None" :wide="true">
    <h2>{{ if (e.id.isDefined) "共有アカウントの編集" else "共有アカウントを追加" }}</h2>
    <form @submit.prevent="saveAccount(s, e)">
      <div class="row">
        <label class="field grow"><span>名前</span><input type="text" v-model="e.name" placeholder="skakapo-bot（GitHub）" required></label>
        <label class="field"><span>ホスト</span>
          <select v-model="e.provider">
            <option v-for="p in GitProvider.all" :value="p">{{ GitProvider.label(p) }}</option></select></label>
      </div>
      <label class="field"><span>API の URL（空なら既定。GitHub Enterprise・Bitbucket Server などのときだけ入れる。Forgejo はリポジトリのホストの /api/v1）</span>
        <input type="url" v-model="e.apiUrl" :placeholder="GitProvider.defaultApiHint(e.provider.v)"></label>
      <label class="field"><span>認証の方式</span>
        <select v-model="e.authKind"><option v-for="k in kinds(e)" :value="k">{{ GitAuthKind.label(k) }}</option></select></label>
      <template v-if="isKind(e, GitAuthKind.Token)">
        <div class="row">
          <label class="field grow"><span>ユーザー名（{{ userHint(e.provider.v) }}）</span><input type="text" v-model="e.username" autocomplete="off"></label>
          <label class="field grow"><span>{{ secretLabel(e) }}</span>
            <input type="password" v-model="e.token" autocomplete="new-password" :required="!e.hasToken"></label>
        </div>
      </template>
      <template v-if="isKind(e, GitAuthKind.GithubApp)">
        <p class="small muted">GitHub App を作り（権限: Contents と Pull requests の Read and write）、対象のリポジトリにインストールします。
          {{ AppNames.self }} は秘密鍵で署名して、そのリポジトリだけに絞った1時間のトークンを都度発行します。個人のトークンは要りません。</p>
        <div class="row">
          <label class="field grow"><span>App ID（または Client ID）</span><input type="text" v-model="e.appId" placeholder="123456" required></label>
          <label class="field grow"><span>Installation ID（空ならリポジトリから自動で探す）</span><input type="text" v-model="e.installationId" inputmode="numeric"></label>
        </div>
        <label class="field"><span>{{ secretLabel(e) }}</span>
          <textarea v-model="e.token" rows="4" class="code" placeholder="-----BEGIN RSA PRIVATE KEY-----" :required="!e.hasToken" autocomplete="off" spellcheck="false"></textarea></label>
      </template>
      <template v-if="isKind(e, GitAuthKind.BitbucketOAuth)">
        <p class="small muted">ワークスペースの設定の「OAuth consumers」で、private consumer（権限: Repositories と Pull requests の Write）を作ります。
          {{ AppNames.self }} はキーとシークレットで、2時間のトークンを都度発行します（client credentials）。</p>
        <div class="row">
          <label class="field grow"><span>キー（Key）</span><input type="text" v-model="e.appId" required autocomplete="off"></label>
          <label class="field grow"><span>{{ secretLabel(e) }}</span>
            <input type="password" v-model="e.token" autocomplete="new-password" :required="!e.hasToken"></label>
        </div>
      </template>
      <div class="row">
        <label class="field grow"><span>コミットの作者の名前（{{ authorHint(e) }}）</span><input type="text" v-model="e.authorName"></label>
        <label class="field grow"><span>コミットの作者のメール</span><input type="email" v-model="e.authorEmail" placeholder="skakapo-bot@example.com"></label>
      </div>
      <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      <div class="actions"><button type="button" @click="s.accountEdit := None">やめる</button><button type="submit" class="primary">保存</button></div>
    </form>
  </Modal>
  <Modal v-for="e in edits(s)" :onClose="_ => s.edit := None" :wide="true">
    <h2>{{ if (e.id.isDefined) "リポジトリの編集" else "リポジトリを追加" }}</h2>
    <form @submit.prevent="save(s, e)">
      <div class="row">
        <label class="field"><span>名前（エージェントが指す名前）</span><input type="text" v-model="e.name" placeholder="skea" required></label>
        <label class="field grow"><span>URL（https）</span><input type="url" v-model="e.url" placeholder="https://github.com/owner/repo" required></label>
      </div>
      <div class="row">
        <label class="field"><span>既定のブランチ</span><input type="text" v-model="e.branch"></label>
        <label class="field grow"><span>関係するサービス（キー、または sBirds の外のサービスの名前）</span>
          <input type="text" v-model="e.service" list="svc-keys"></label>
        <datalist id="svc-keys"><option v-for="k in serviceKeys(s)" :value="k"></option></datalist>
      </div>
      <label class="field"><span>push・プルリクエストの名義</span>
        <select v-model="e.account"><option value="">利用者の GitHub との連携（開発者向け）</option>
          <option v-for="a in s.accounts" :value="a.id.toString">共有アカウント: {{ a.name }}</option></select></label>
      <label class="field"><span>説明</span><textarea v-model="e.description" rows="2"></textarea></label>
      <label class="field"><span>作業の決まり（ビルド・テストの方法、ブランチやコミットの決まり、触ってはいけない場所など）</span><textarea v-model="e.notes" rows="6"></textarea></label>
      <label class="row"><input type="checkbox" v-model="e.enabled"> 有効</label>
      <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      <div class="actions"><button type="button" @click="s.edit := None">やめる</button><button type="submit" class="primary">保存</button></div>
    </form>
  </Modal>
</template>

<style scoped>
.panel + .panel { margin-top: 1.2rem; }
h3 { margin: 0 0 .2rem; }
</style>

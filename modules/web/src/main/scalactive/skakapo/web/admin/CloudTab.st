<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*

import java.util.UUID

// クラウド（AWS・Google Cloud・Azure）の資格情報: エージェントが cloud_request でクラウドの API を呼ぶときに使う.
// 入力の項目はクラウドの種類ごとの定義（CloudProvider.configFields・secretFields）から作る. 秘密は画面に戻さない
case class Edit(id: Option[UUID], name: RV[String], provider: RV[String], config: RV[Map[String, String]], secrets: RV[Map[String, String]],
                hasSecret: Boolean, notes: RV[String], enabled: RV[Boolean], access: RV[String], groups: RV[List[UUID]])

case class State(list: RV[List[CloudCredentialDto]] = RV(Nil), userGroups: RV[List[UserGroupDto]] = RV(Nil), edit: RV[Option[Edit]] = RV(None), error: RV[String] = RV(""))

def initState() = State()

def mounted(s: State): Unit = load(s)

def load(s: State): Unit = {
  Toasts.report(Api.cloudCredentials())(xs => s.list := xs)
  Toasts.report(Api.userGroups())(gs => s.userGroups := gs)
}

def open(s: State, x: Option[CloudCredentialDto]): Unit = {
  s.error := ""
  s.edit := Some(x.fold(Edit(None, RV(""), RV(CloudProvider.Aws), RV(Map.empty), RV(Map.empty), false, RV(""), RV(true), RV(CloudProvider.AccessAdmins), RV(Nil)))(c =>
    Edit(Some(c.id), RV(c.name), RV(c.provider), RV(c.config), RV(Map.empty), c.hasSecret, RV(c.notes), RV(c.enabled), RV(c.access), RV(c.groups))))
}

def get(m: RV[Map[String, String]], k: String): String = m.v.getOrElse(k, "")
def set(m: RV[Map[String, String]], k: String, v: String): Unit = m := m.v + (k -> v)

def configFields(e: Edit) = e.provider.mapC(CloudProvider.configFields)
def secretFields(e: Edit) = e.provider.mapC(CloudProvider.secretFields)
/** 秘密は、どれかを入れたときだけ送る（入れなければ保存してあるものを使う）. */
def enteredSecrets(e: Edit): Option[Map[String, String]] = Option(e.secrets.v.filter(_._2.trim.nonEmpty)).filter(_.nonEmpty)

def save(s: State, e: Edit): Unit =
  Store.saved(Api.saveCloudCredential(e.id, CloudCredentialReq(e.name.v.trim, e.provider.v, e.config.v, enteredSecrets(e), e.notes.v, e.enabled.v, e.access.v, e.groups.v)),
    s.error, (c: CloudCredentialDto) => s"保存しました（${c.identity.getOrElse("認証できました")}）") { s.edit := None; load(s) }

def remove(s: State, x: CloudCredentialDto): Unit = Dialog.confirm(s"${x.name} を削除しますか？（エージェントはこのクラウドを使えなくなります）", "削除") {
  Toasts.report(Api.deleteCloudCredential(x.id))(_ => load(s))
}

def summary(c: CloudCredentialDto): String =
  CloudProvider.configFields(c.provider).filterNot(_._1 == CloudProvider.AllowedHosts).flatMap((k, _, _) => c.config.get(k).filter(_.nonEmpty)).mkString("・")
def edits(s: State) = s.edit.mapC(_.toList)
/** 使える人の表示（種類で選んだときは、その種類の名前）. */
def accessText(s: State, c: CloudCredentialDto): String =
  if (c.access != CloudProvider.AccessGroups) CloudProvider.accessLabel(c.access)
  else s.userGroups.v.filter(g => c.groups.contains(g.id)).map(_.label) match {
    case Nil => "管理者の依頼だけ（種類が選ばれていません）"
    case ls  => ls.mkString("・") + "・管理者"
  }
def chosen(e: Edit, g: UserGroupDto): Boolean = e.groups.v.contains(g.id)
def toggle(e: Edit, g: UserGroupDto): Unit = e.groups := (if (chosen(e, g)) e.groups.v.filterNot(_ == g.id) else e.groups.v :+ g.id)
def byGroups(e: Edit) = e.access.mapC(_ == CloudProvider.AccessGroups)
def secretLabel(e: Edit, label: String): String = if (e.hasSecret) s"$label（変えるときだけ入れる）" else label
</script>

<template>
  <div class="panel">
    <div class="row head">
      <p class="grow small muted">エージェントは cloud_request で、ここに登録した資格情報を使ってクラウドの API を呼びます（読み取りも変更も、資格情報の権限のとおり）。
        権限は、クラウド側の IAM（ポリシー・ロール）で必要な範囲に絞ってください。資格情報は暗号化して保存し、画面には戻しません。</p>
      <button class="primary" @click="open(s, None)">＋ 資格情報を追加</button>
    </div>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>名前</th><th>クラウド</th><th>設定</th><th>使える人</th><th>状態</th><th></th></tr></thead>
        <tbody>
          <tr v-for="x in s.list" class="click" @click="open(s, Some(x))">
            <td><b>{{ x.name }}</b></td>
            <td>{{ CloudProvider.label(x.provider) }}</td>
            <td class="small">{{ summary(x) }}</td>
            <td class="small">{{ accessText(s, x) }}</td>
            <td><span :class='if (x.enabled) "st st-done" else "st"'>{{ if (x.enabled) "有効" else "無効" }}</span></td>
            <td><button class="ghost small danger" @click.stop="remove(s, x)">削除</button></td>
          </tr>
        </tbody>
      </table>
    </div>
    <p v-if="s.list.mapC(_.isEmpty)" class="empty">クラウドの資格情報はまだありません。</p>
  </div>
  <Modal v-for="e in edits(s)" :onClose="_ => s.edit := None" :wide="true">
    <h2>{{ if (e.id.isDefined) "資格情報の編集" else "資格情報を追加" }}</h2>
    <form @submit.prevent="save(s, e)">
      <div class="row">
        <label class="field grow"><span>名前（エージェントが指す名前）</span><input type="text" v-model="e.name" placeholder="aws-production" required></label>
        <label class="field"><span>クラウド</span>
          <select v-model="e.provider"><option v-for="p in CloudProvider.all" :value="p">{{ CloudProvider.label(p) }}</option></select></label>
      </div>
      <label v-for="f in configFields(e)" class="field"><span>{{ f._2 }}</span>
        <input type="text" :value="get(e.config, f._1)" @input="set(e.config, f._1, Dom.value(event))" :required="f._3" autocomplete="off" spellcheck="false"></label>
      <label v-for="f in secretFields(e)" class="field"><span>{{ secretLabel(e, f._2) }}</span>
        <textarea v-if='f._1 == "serviceAccountJson"' class="code" rows="5" :value="get(e.secrets, f._1)" @input="set(e.secrets, f._1, Dom.value(event))"
          :required="f._3 && !e.hasSecret" autocomplete="off" spellcheck="false" placeholder='{"type": "service_account", ...}'></textarea>
        <input v-else type="password" :value="get(e.secrets, f._1)" @input="set(e.secrets, f._1, Dom.value(event))"
          :required="f._3 && !e.hasSecret" autocomplete="new-password"></label>
      <label class="field"><span>説明・決まり（エージェントに伝える. 例: 本番の環境. 読み取りの調査だけに使い、変更は頼まれたときだけ）</span>
        <textarea v-model="e.notes" rows="3"></textarea></label>
      <label class="field"><span>使える人（この資格情報を、だれの依頼でエージェントに使わせるか）</span>
        <select v-model="e.access"><option v-for="a in CloudProvider.accessAll" :value="a">{{ CloudProvider.accessLabel(a) }}</option></select></label>
      <div v-if="byGroups(e).v" class="field">
        <span>使える利用者の種類（管理者はいつも使えます）</span>
        <div class="row groups">
          <label v-for="g in s.userGroups" class="row"><input type="checkbox" :checked="chosen(e, g)" @change="toggle(e, g)"> {{ g.label }}</label>
        </div>
        <p v-if="s.userGroups.mapC(_.isEmpty)" class="small muted">利用者の種類がまだありません。「利用者の種類」のタブで作ってください。</p>
      </div>
      <label class="row"><input type="checkbox" v-model="e.enabled"> 有効（エージェントが使える）</label>
      <p class="small muted">保存するときに、実際に認証できるか確かめます（AWS は STS で呼び出し元を確かめ、Google Cloud・Azure はトークンを発行してみます）。</p>
      <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      <div class="actions"><button type="button" @click="s.edit := None">やめる</button><button type="submit" class="primary">保存</button></div>
    </form>
  </Modal>
</template>


<style scoped>
.groups { gap: .4rem 1.2rem; }
</style>

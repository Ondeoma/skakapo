<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*

import java.util.UUID

// 利用者の種類（engineer・operator など）: 種類ごとの回答の決まりを、その人の会話の前提に入れる.
// 種類の決め方: 管理者がその人に付けた種類 → IdP のグループに対応する種類（並び順） → 既定の種類
case class Edit(id: Option[UUID], key: RV[String], label: RV[String], description: RV[String], instructions: RV[String], idpGroups: RV[String],
                isDefault: RV[Boolean], position: RV[String])

case class State(groups: RV[List[UserGroupDto]] = RV(Nil), users: RV[List[AdminUserDto]] = RV(Nil), edit: RV[Option[Edit]] = RV(None),
                 error: RV[String] = RV(""))

def initState() = State()

def mounted(s: State): Unit = load(s)

def load(s: State): Unit = {
  Toasts.report(Api.userGroups())(xs => s.groups := xs)
  Toasts.report(Api.adminUsers())(xs => s.users := xs)
}

def open(s: State, x: Option[UserGroupDto]): Unit = {
  s.error := ""
  s.edit := Some(x.fold(Edit(None, RV(""), RV(""), RV(""), RV(""), RV(""), RV(false), RV((s.groups.v.size + 1).toString)))(g =>
    Edit(Some(g.id), RV(g.key), RV(g.label), RV(g.description), RV(g.instructions), RV(g.idpGroups.mkString(", ")), RV(g.isDefault), RV(g.position.toString))))
}

def save(s: State, e: Edit): Unit =
  Store.saved(Api.saveUserGroup(e.id, UserGroupReq(e.key.v.trim, e.label.v.trim, e.description.v, e.instructions.v,
    e.idpGroups.v.split(",").map(_.trim).filter(_.nonEmpty).toList, e.isDefault.v, e.position.v.toIntOption.getOrElse(0))), s.error) { s.edit := None; load(s) }

def remove(s: State, x: UserGroupDto): Unit = Dialog.confirm(s"「${x.label}」を削除しますか？（この種類を付けた人は、IdP のグループ・既定で決まります）", "削除") {
  Toasts.report(Api.deleteUserGroup(x.id))(_ => load(s))
}

/** 利用者に種類を付ける（空なら IdP のグループ・既定で決める）. */
def assign(s: State, u: AdminUserDto, value: String): Unit =
  Toasts.report(Api.assignGroup(u.user.id, Option(value).filter(_.nonEmpty).map(UUID.fromString)))(_ => { Toasts.show("変えました"); load(s) })

def assignedValue(u: AdminUserDto): String = u.assigned.fold("")(_.toString)
def edits(s: State) = s.edit.mapC(_.toList)
</script>

<template>
  <div class="panel">
    <div class="row head">
      <p class="grow small muted">利用者の種類ごとに、エージェントの回答の決まりを決めます（例: エンジニアにはソースコードの詳しい説明、オペレータにはタスクを作って担当に知らせたことを短く）。
        種類は、管理者がその人に付けた種類 → IdP のグループに対応する種類（並び順） → 既定の種類 の順で決まります。</p>
      <button class="primary" @click="open(s, None)">＋ 種類を追加</button>
    </div>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>種類</th><th>キー</th><th>IdP のグループ</th><th>既定</th><th></th></tr></thead>
        <tbody>
          <tr v-for="g in s.groups" class="click" @click="open(s, Some(g))">
            <td><b>{{ g.label }}</b> <span class="small muted">{{ g.description }}</span></td>
            <td class="small">{{ g.key }}</td>
            <td class="small">{{ g.idpGroups.mkString("・") }}</td>
            <td><span v-if="g.isDefault" class="st st-done">既定</span></td>
            <td><button class="ghost small danger" @click.stop="remove(s, g)">削除</button></td>
          </tr>
        </tbody>
      </table>
    </div>
    <p v-if="s.groups.mapC(_.isEmpty)" class="empty">利用者の種類はまだありません。</p>
  </div>
  <div class="panel">
    <h3>利用者</h3>
    <p class="small muted">「IdP のグループ・既定で決める」にすると、ログインのたびに IdP のグループから種類が決まります。</p>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>名前</th><th>IdP のグループ</th><th>種類（付ける）</th><th>実際の種類</th></tr></thead>
        <tbody>
          <tr v-for="u in s.users">
            <td><b>{{ u.user.name }}</b> <span class="small muted">{{ u.user.email.getOrElse("") }}</span></td>
            <td class="small">{{ u.idpGroups.mkString("・") }}</td>
            <td>
              <select :value="assignedValue(u)" @change="assign(s, u, Dom.value(event))">
                <option value="">IdP のグループ・既定で決める</option>
                <option v-for="g in s.groups" :value="g.id.toString">{{ g.label }}</option>
              </select>
            </td>
            <td class="small">{{ u.effective.getOrElse("（なし）") }}</td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
  <Modal v-for="e in edits(s)" :onClose="_ => s.edit := None" :wide="true">
    <h2>{{ if (e.id.isDefined) "種類の編集" else "種類を追加" }}</h2>
    <form @submit.prevent="save(s, e)">
      <div class="row">
        <label class="field"><span>キー（英小文字）</span><input type="text" v-model="e.key" placeholder="engineer" required></label>
        <label class="field grow"><span>表示名</span><input type="text" v-model="e.label" placeholder="エンジニア" required></label>
        <label class="field"><span>並び順</span><input type="number" v-model="e.position"></label>
      </div>
      <label class="field"><span>説明</span><input type="text" v-model="e.description" placeholder="開発者・運用の担当者"></label>
      <label class="field"><span>回答の決まり（エージェントに伝える. 作業の進め方・返事の書き方）</span>
        <textarea v-model="e.instructions" rows="8" placeholder="- 専門用語やコードの細部は書かず、したことと次に何が起きるかを短く伝える"></textarea></label>
      <label class="field"><span>対応する IdP のグループ（カンマ区切り. ログインした人がこのグループにいれば、この種類になる）</span>
        <input type="text" v-model="e.idpGroups" placeholder="skakapo-engineers, dev-team"></label>
      <label class="row"><input type="checkbox" v-model="e.isDefault"> 既定（どれにも当てはまらない人の種類. 1つだけ）</label>
      <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      <div class="actions"><button type="button" @click="s.edit := None">やめる</button><button type="submit" class="primary">保存</button></div>
    </form>
  </Modal>
</template>

<style scoped>
h3 { margin: 0 0 .2rem; }
.panel + .panel { margin-top: 1.2rem; }
</style>

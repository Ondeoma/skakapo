<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*

import java.util.UUID

// 依頼の種類: 会話の最初のフォームの項目と、エージェントへの手順（例: 不具合なら skea に起票して、修正のプルリクエストを出し、結果をコメントする）
case class Row(name: RV[String], label: RV[String], kind: RV[String], required: RV[Boolean], options: RV[String], placeholder: RV[String])

case class Edit(id: Option[UUID], key: RV[String], label: RV[String], icon: RV[String], description: RV[String], instructions: RV[String],
                position: RV[String], enabled: RV[Boolean], rows: RV[List[Row]])

case class State(list: RV[List[RequestTypeDto]] = RV(Nil), edit: RV[Option[Edit]] = RV(None), error: RV[String] = RV(""))

def initState() = State()

def mounted(s: State): Unit = load(s)

def load(s: State): Unit = Store.report(Api.adminRequestTypes())(xs => s.list := xs)

def row(f: FormField): Row = Row(RV(f.name), RV(f.label), RV(f.kind), RV(f.required), RV(f.options.mkString(", ")), RV(f.placeholder))

def open(s: State, x: Option[RequestTypeDto]): Unit = {
  s.error := ""
  val e = x match {
    case Some(t) =>
      Edit(Some(t.id), RV(t.key), RV(t.label), RV(t.icon), RV(t.description), RV(t.instructions), RV(t.position.toString), RV(t.enabled), RV(t.fields.map(row)))
    case None =>
      val rows = List(row(FormField("title", "件名", required = true)), row(FormField("description", "内容", FieldKind.Textarea, required = true)))
      Edit(None, RV(""), RV(""), RV("📝"), RV(""), RV(""), RV((s.list.v.size + 1).toString), RV(true), RV(rows))
  }
  s.edit := Some(e)
}

def addRow(e: Edit): Unit = e.rows := e.rows.v :+ row(FormField(s"field${e.rows.v.size + 1}", "項目"))
def removeRow(e: Edit, r: Row): Unit = e.rows := e.rows.v.filterNot(_ eq r)
def up(e: Edit, r: Row): Unit = {
  val i = e.rows.v.indexWhere(_ eq r)
  if (i > 0) e.rows := e.rows.v.updated(i, e.rows.v(i - 1)).updated(i - 1, r)
}

def fields(e: Edit): List[FormField] = e.rows.v.map(r => FormField(r.name.v.trim, r.label.v.trim, r.kind.v, r.required.v,
  if (r.kind.v == FieldKind.Select) r.options.v.split("[,、]").toList.map(_.trim).filter(_.nonEmpty) else Nil, r.placeholder.v))

def save(s: State, e: Edit): Unit =
  Store.saved(Api.saveRequestType(e.id, RequestTypeReq(e.key.v.trim, e.label.v.trim, e.description.v, e.icon.v.trim, fields(e), e.instructions.v,
    e.position.v.toIntOption.getOrElse(0), e.enabled.v)), s.error) { s.edit := None; load(s) }

def remove(s: State, x: RequestTypeDto): Unit = Dialog.confirm(s"「${x.label}」を削除しますか？（この種類の会話は残ります）", "削除") {
  Store.report(Api.deleteRequestType(x.id))(_ => load(s))
}

def edits(s: State) = s.edit.mapC(_.toList)
val vars: String = "{" + "{tracker.service}}・{" + "{tracker.project}}・{" + "{form.<項目の名前>}}"
</script>

<template>
  <div class="panel">
    <div class="row head">
      <p class="grow small muted">利用者は、会話の最初にここで決めたフォームに入れます。手順はエージェントへの指示です。</p>
      <button class="primary" @click="open(s, None)">＋ 依頼の種類を追加</button>
    </div>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th></th><th>表示名</th><th>キー</th><th>項目</th><th>状態</th><th></th></tr></thead>
        <tbody>
          <tr v-for="x in s.list" class="click" @click="open(s, Some(x))">
            <td>{{ x.icon }}</td>
            <td><b>{{ x.label }}</b><br><span class="small muted">{{ x.description }}</span></td>
            <td>{{ x.key }}</td>
            <td class="small">{{ x.fields.map(_.label).mkString("・") }}</td>
            <td><span :class='if (x.enabled) "st st-done" else "st"'>{{ if (x.enabled) "有効" else "無効" }}</span></td>
            <td><button class="ghost small danger" @click.stop="remove(s, x)">削除</button></td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
  <Modal v-for="e in edits(s)" :onClose="_ => s.edit := None" :xwide="true">
    <h2>{{ if (e.id.isDefined) "依頼の種類の編集" else "依頼の種類を追加" }}</h2>
    <form @submit.prevent="save(s, e)">
      <div class="row">
        <label class="field"><span>アイコン</span><input type="text" v-model="e.icon" maxlength="8" class="icon-in"></label>
        <label class="field grow"><span>表示名</span><input type="text" v-model="e.label" required></label>
        <label class="field"><span>キー</span><input type="text" v-model="e.key" placeholder="bug" required></label>
        <label class="field"><span>並び順</span><input type="number" v-model="e.position"></label>
      </div>
      <label class="field"><span>説明（選ぶときに出す）</span><input type="text" v-model="e.description" maxlength="1000"></label>
      <h3>フォームの項目</h3>
      <div class="table-wrap">
        <table class="list rows">
          <thead><tr><th>名前（英小文字）</th><th>表示名</th><th>種類</th><th>必須</th><th>選択肢（, 区切り）</th><th>例</th><th></th></tr></thead>
          <tbody>
            <tr v-for="r in e.rows">
              <td><input type="text" v-model="r.name"></td>
              <td><input type="text" v-model="r.label"></td>
              <td><select v-model="r.kind"><option v-for="k in FieldKind.all" :value="k">{{ FieldKind.label(k) }}</option></select></td>
              <td><input type="checkbox" v-model="r.required"></td>
              <td><input type="text" v-model="r.options" :disabled='r.kind.v != "select"'></td>
              <td><input type="text" v-model="r.placeholder"></td>
              <td class="nowrap"><button type="button" class="ghost small" @click="up(e, r)">↑</button><button type="button" class="ghost small danger" @click="removeRow(e, r)">×</button></td>
            </tr>
          </tbody>
        </table>
      </div>
      <p><button type="button" class="ghost small" @click="addRow(e)">＋ 項目を足す</button></p>
      <label class="field"><span>エージェントへの手順（<code>{{ vars }}</code> で設定や入力を差し込める）</span>
        <textarea class="code" v-model="e.instructions" rows="12"></textarea></label>
      <label class="row"><input type="checkbox" v-model="e.enabled"> 有効（新しい依頼の画面に出す）</label>
      <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      <div class="actions"><button type="button" @click="s.edit := None">やめる</button><button type="submit" class="primary">保存</button></div>
    </form>
  </Modal>
</template>

<style scoped>
h3 { margin: 1rem 0 .3rem; font-size: .95rem; }
.rows input[type="text"], .rows select { width: 100%; min-width: 6rem; }
.icon-in { width: 4rem; text-align: center; }
</style>

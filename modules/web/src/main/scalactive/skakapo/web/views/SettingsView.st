<script lang="scala">
import skakapo.web.*
import skakapo.shared.*
import skakapo.web.parts.*

import org.scalajs.dom

import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

// 設定: アイコン・名前・エージェントへの自分用の前提（サーバー）、画面の色・プッシュ通知（この端末）、API トークン・MCP
case class State(name: RV[String] = RV(Store.me.v.map(_.name).getOrElse("")),
                 theme: RV[String] = RV(Theme.current.v), context: RV[String] = RV(""))

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("設定")
  watch(s.theme)((_, v) => Theme.set(v))
  Store.report(Api.userContext())(t => s.context := t)
}

def saveContext(s: State): Unit = Store.report(Api.saveUserContext(s.context.v))(t => { s.context := t; Store.toast("保存しました") })

def save(s: State): Unit =
  Store.report(Api.updateMe(UpdateMeReq(Some(s.name.v.trim), None))) { me =>
    Store.me := Some(me)
    Store.toast("保存しました")
  }

// ---------------------------------------------------------------- アイコン

val AvatarPx = 256

def meBrief = Store.me.mapC(_.map(_.brief))
def uploaded = Store.me.mapC(_.exists(_.avatarUploaded))

/** 画像の中央を正方形に切り抜き、256×256 にする（WebP. 対応していないブラウザでは PNG）. */
def square(file: dom.File): Future[dom.Blob] = {
  js.Dynamic.global.createImageBitmap(file).asInstanceOf[js.Promise[js.Dynamic]].toFuture.flatMap { img =>
    val (w, h) = (img.width.asInstanceOf[Double], img.height.asInstanceOf[Double])
    val side = math.min(w, h)
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    canvas.width = AvatarPx; canvas.height = AvatarPx
    val ctx = canvas.getContext("2d").asInstanceOf[js.Dynamic]
    ctx.imageSmoothingQuality = "high"
    ctx.drawImage(img, (w - side) / 2, (h - side) / 2, side, side, 0, 0, AvatarPx, AvatarPx)
    val p = Promise[dom.Blob]()
    canvas.asInstanceOf[js.Dynamic].toBlob((b: dom.Blob) =>
      if (b == null) p.failure(new Exception("画像を作れませんでした")) else p.success(b), "image/webp", 0.9)
    p.future
  }
}

def avatarDone(me: MeDto, msg: String): Unit = {
  Store.me := Some(me)
  Store.toast(msg)
}

def pickAvatar(e: dom.Event): Unit = {
  val input = e.target.asInstanceOf[dom.HTMLInputElement]
  Option(input.files).filter(_.length > 0).map(_(0)).foreach { f =>
    input.value = "" // 同じファイルを選び直せるように
    if (!f.`type`.startsWith("image/")) Store.toast("画像のファイルを選んでください", error = true)
    else Store.report(square(f).flatMap(Api.uploadAvatar))(me => avatarDone(me, "アイコンを保存しました"))
  }
}

def removeAvatar(): Unit =
  Dialog.confirm("保存したアイコンを削除しますか？（ログインの仕組みから受け取った画像か、名前の1文字目の表示に戻ります）", "削除") {
    Store.report(Api.deleteAvatar())(me => avatarDone(me, "アイコンを削除しました"))
  }


def details = Store.showDetails
def setDetails(on: Boolean): Unit =
  Store.report(Api.updateMe(UpdateMeReq(None, None, Some(on)))) { me => Store.me := Some(me); Store.toast(if (on) "細かい進捗を表示します" else "細かい進捗を表示しません") }
</script>

<template>
  <div class="page narrow">
    <h1>設定</h1>
    <section class="panel">
      <h2>アカウント</h2>
      <div class="field"><span>アイコン</span>
        <div class="row avatar-row">
          <Avatar :user="meBrief.v" :size="64" />
          <label class="btn">画像を選ぶ<input type="file" accept="image/png,image/jpeg,image/gif,image/webp" class="hidden-file" @change="pickAvatar(event)"></label>
          <button v-if="uploaded" type="button" class="ghost" @click="removeAvatar()">削除</button>
        </div>
        <span class="muted small">中央を正方形に切り抜いて保存します。保存しなければ、ログインの仕組み（IdP）の画像を使います。</span>
      </div>
      <form @submit.prevent="save(s)">
        <label class="field"><span>表示名</span><input type="text" v-model="s.name" maxlength="100"></label>
        <button type="submit" class="primary">保存</button>
      </form>
    </section>
    <section class="panel">
      <h2>エージェントへの前提（自分用）</h2>
      <p class="small muted">あなたの会話で、エージェントが最初に読みます（管理者が決めた前提のあとに）。担当しているサービス、よく使うプロジェクト、報告の好みなどを書いておくと、毎回説明しなくて済みます。</p>
      <form @submit.prevent="saveContext(s)">
        <label class="field"><textarea class="code" v-model="s.context" rows="6" placeholder="- 担当は経理システム（skea のプロジェクト ACC）&#10;- 報告は箇条書きで短く"></textarea></label>
        <button type="submit" class="primary">保存</button>
      </form>
    </section>
    <section class="panel">
      <h2>依頼の画面</h2>
      <label class="row"><input type="checkbox" :checked="details.v" @change="setDetails(Dom.checked(event))">
        細かい進捗を表示する（エージェントのツールの呼び出しと結果・作業の状態）</label>
      <p class="small muted">表示しないときは、エージェントの返事と、作業中であることだけを出します。どの端末でも同じ設定になります。</p>
    </section>
    <section class="panel">
      <h2>表示（この端末）</h2>
      <label class="field"><span>画面の色</span>
        <select v-model="s.theme"><option v-for="t in Theme.options" :value="t._1">{{ t._2 }}</option></select></label>
    </section>
    <section class="panel">
      <h2>プッシュ通知</h2>
      <p class="small">受け取ると、画面を閉じていても、エージェントが返事をした・失敗したときにこの端末に届きます。</p>
      <p><PushToggle /></p>
    </section>
    <ApiTokenSettings />
  </div>
</template>

<style scoped>
.avatar-row { align-items: center; gap: .8rem; }
.hidden-file { display: none; }
</style>

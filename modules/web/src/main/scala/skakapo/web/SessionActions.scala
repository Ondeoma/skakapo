package skakapo.web

/** 依頼（会話）のアーカイブと削除（一覧と個別の画面で共通）. done は終わったあと（一覧の読み直しなど）. */
object SessionActions {
  def archive(
    id: java.util.UUID,
    archived: Boolean
  )(
    done: => Unit
  ): Unit =
    Store.report(Api.archive(id, archived))(_ => {
      Store.toast(if (archived) "アーカイブしました" else "アーカイブを解除しました"); Store.refresh(); done
    })

  def remove(
    id: java.util.UUID,
    title: String
  )(
    done: => Unit
  ): Unit =
    Dialog.confirm(s"「$title」を削除しますか？（元に戻せません）", "削除") {
      Store.report(Api.deleteSession(id))(_ => { Store.toast("削除しました"); Store.refresh(); done })
    }
}

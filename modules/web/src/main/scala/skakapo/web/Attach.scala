package skakapo.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skakapo.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * 添付するファイル（新しい依頼・会話の返事で共通）. 選ぶ・貼り付ける・落とすで上げ、上げたものを atts に足す.
 * 送ったら clear で空にする. 種類は問わない（Claude に渡せない種類は、名前と種類だけがエージェントに伝わる）.
 */
final case class Attach(atts: RV[List[AttachmentDto]] = RV(Nil), uploading: RV[Int] = RV(0)) {

  def busy: Boolean = uploading.v > 0
  def ids: List[java.util.UUID] = atts.v.map(_.id)
  def clear(): Unit = atts := Nil
  def remove(a: AttachmentDto): Unit = atts := atts.v.filterNot(_.id == a.id)

  def upload(files: List[dom.File]): Unit = files.foreach { f =>
    val name = if (f.name.nonEmpty) f.name else "file"
    if (atts.v.size + uploading.v >= AttachmentRule.MaxCount)
      Toasts.show(s"添付は1回に${AttachmentRule.MaxCount}個までです", error = true)
    else AttachmentRule.check(f.`type`, f.size.toLong) match {
      case Left(e) => Toasts.show(s"$name: $e", error = true)
      case Right(_) =>
        uploading := uploading.v + 1
        Api.uploadAttachment(f, name).onComplete { r =>
          uploading := uploading.v - 1
          r.fold(Toasts.error, a => atts := atts.v :+ a)
        }
    }
  }

  /** 選んだファイル（同じファイルを選び直せるよう、選んだ欄は空に戻る）. */
  def pick(e: dom.Event): Unit = upload(Dom.takeFiles(e))

  /** 貼り付けたファイル（スクリーンショットなど）. 文字の貼り付けはそのまま. */
  def paste(e: dom.Event): Unit = {
    val items = e.asInstanceOf[dom.ClipboardEvent].clipboardData.items
    val files = (0 until items.length).map(items(_)).filter(_.kind == "file").flatMap(i => Option(i.getAsFile())).toList
    if (files.nonEmpty) { e.preventDefault(); upload(files) }
  }

  def drop(e: dom.Event): Unit = {
    val fs = Dom.droppedFiles(e.asInstanceOf[dom.DragEvent])
    if (fs.nonEmpty) { e.preventDefault(); upload(fs) }
  }
}

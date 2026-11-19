package skakapo.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skakapo.shared.*

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}

final case class Toast(
  id: Int,
  text: String,
  error: Boolean)

/** 画面全体の状態. 会話の一覧は20秒ごとに読み直す（画面を開いている間）. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val sessions: RV[List[SessionDto]] = RV(Nil)

  /** 今月のクレジット（自分と全体の残り） */
  val credit: RV[Option[CreditDto]] = RV(None)
  val loading: RV[Boolean] = RV(false)
  val toasts: RV[List[Toast]] = RV(Nil)

  // ---------------------------------------------------------------- トースト

  private var toastSeq = 0

  def toast(
    text: String,
    error: Boolean = false
  ): Unit = {
    toastSeq += 1
    val t = Toast(toastSeq, text, error)
    toasts := (toasts.v :+ t).takeRight(4)
    dom.window.setTimeout(() => dismiss(t.id), if (error) 9000 else 5000)
  }

  def dismiss(
    id: Int
  ): Unit = toasts := toasts.v.filterNot(_.id == id)

  def error(
    e: Throwable
  ): Unit = e match {
    case ApiError(401, _) => ()
    case other => toast(other.getMessage, error = true)
  }

  /** 編集の画面から保存する: 失敗なら err に理由、成功なら「保存しました」を出して done. */
  def saved[A](
    f: Future[A],
    err: RV[String],
    message: A => String = (_: A) => "保存しました"
  )(
    done: => Unit
  ): Unit = f.onComplete {
    case Success(a) => toast(message(a)); done
    case Failure(e) => err := e.getMessage
  }

  /** 依頼の画面で、細かい進捗（ツールの呼び出しと結果・作業の状態）を出すか（利用者の設定. 既定は出さない）. */
  val showDetails = me.mapC(_.exists(_.showDetails))

  /** Future の失敗をトーストで知らせる. */
  def report[A](
    f: Future[A]
  )(
    ok: A => Unit
  ): Unit = f.onComplete {
    case Success(a) => ok(a)
    case Failure(e) => error(e)
  }

  def go(
    path: String
  ): Unit = Router.go(path)

  // ---------------------------------------------------------------- 会話

  def refresh(): Unit = if (me.v.isDefined) {
    Api.sessions().foreach(ss => sessions := ss)
    Api.credit().foreach(c => credit := Some(c))
  }

  private var timer: Option[Int] = None

  /** 定期的に読み直す（タブが裏にある間は止める）. */
  def start(): Unit = if (timer.isEmpty) {
    refresh()
    timer = Some(dom.window.setInterval(
      () => if (dom.document.visibilityState.asInstanceOf[String] == "visible") refresh(),
      20000
    ))
    dom.document.addEventListener(
      "visibilitychange",
      (_: dom.Event) => if (dom.document.visibilityState.asInstanceOf[String] == "visible") refresh()
    )
  }

  /** 作業中（順番待ち・作業中）の依頼の数（ヘッダーの印）. */
  val busyCount = sessions.mapC(_.count(s => SessionStatus.busy(s.status)))
}

/** URL のクエリ. */
object Query {
  def get(
    k: String
  ): Option[String] = Option(new dom.URL(dom.window.location.href).searchParams.get(k)).filter(_.nonEmpty)
}

/** DOM の補助. */
object Dom {
  def value(
    e: dom.Event
  ): String = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
  def checked(
    e: dom.Event
  ): Boolean = e.target.asInstanceOf[dom.HTMLInputElement].checked

  def copy(
    text: String,
    message: String = "コピーしました"
  ): Unit = {
    dom.window.navigator.clipboard.writeText(text)
    Store.toast(message)
  }
}

/** 画面内のダイアログ（ブラウザ標準の confirm・prompt の代わり）. 表示は parts/DialogHost.st. */
final case class DialogReq(
  message: String,
  okLabel: String,
  danger: Boolean,
  input: Option[RV[String]],
  choices: List[(String, String)],
  onOk: String => Unit,
  onCancel: () => Unit)

/** この端末だけの表示の設定（localStorage. 使えないときは既定値）. */
object Prefs {
  def get(
    key: String,
    default: String
  ): String =
    scala.util.Try(Option(dom.window.localStorage.getItem("skakapo." + key))).toOption.flatten.getOrElse(default)

  def set(
    key: String,
    value: String
  ): Unit = scala.util.Try(dom.window.localStorage.setItem("skakapo." + key, value))
}

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme {
  val current: RV[String] = RV(Prefs.get("theme", "system"))

  def apply(): Unit = {
    val el = dom.document.documentElement
    if (current.v == "system") el.removeAttribute("data-theme") else el.setAttribute("data-theme", current.v)
  }

  def set(
    v: String
  ): Unit = { current := v; Prefs.set("theme", v); apply() }

  val options: List[(String, String)] = List("system" -> "自動（OS に合わせる）", "light" -> "ライト", "dark" -> "ダーク")
}

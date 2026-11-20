package skakapo.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import skakapo.shared.*

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

/** 画面全体の状態. 会話の一覧は20秒ごとに読み直す（画面を開いている間）. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val sessions: RV[List[SessionDto]] = RV(Nil)

  /** 今月のクレジット（自分と全体の残り） */
  val credit: RV[Option[CreditDto]] = RV(None)
  val loading: RV[Boolean] = RV(false)

  /** 編集の画面から保存する: 失敗なら err に理由、成功なら「保存しました」を出して done. */
  def saved[A](f: Future[A], err: RV[String], message: A => String = (_: A) => "保存しました")(done: => Unit): Unit =
    f.onComplete {
      case Success(a) => Toasts.show(message(a)); done
      case Failure(e) => err := e.getMessage
    }

  /** 依頼の画面で、細かい進捗（ツールの呼び出しと結果・作業の状態）を出すか（利用者の設定. 既定は出さない）. */
  val showDetails = me.mapC(_.exists(_.showDetails))

  def go(path: String): Unit = Router.go(path)

  // ---------------------------------------------------------------- 会話

  def refresh(): Unit = if (me.v.isDefined) {
    Api.sessions().foreach(ss => sessions := ss)
    Api.credit().foreach(c => credit := Some(c))
  }

  private var polling: Option[EffectHandle] = None

  /** すぐ読み、あとは定期的に読み直す（タブが裏にある間は止め、戻ったらすぐ読む）. */
  def start(): Unit = if (polling.isEmpty) {
    refresh()
    polling = Some(useVisiblePolling(20000)(refresh()))
  }

  /** 作業中（順番待ち・作業中）の依頼の数（ヘッダーの印）. */
  val busyCount = sessions.mapC(_.count(s => SessionStatus.busy(s.status)))
}

/** アイコン（sbirds-lib の parts/Avatar.st）に出す人. */
extension (u: UserBrief) {
  def person: Person = Person(u.id.toString, u.name, u.avatarUrl)
}

/** この端末だけの表示の設定（localStorage. 使えないときは既定値）. */
object Prefs extends sbirds.web.LocalPrefs("skakapo")

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme extends sbirds.web.ThemeSwitch(Prefs, sbirds.web.ThemeSwitch.Plain)

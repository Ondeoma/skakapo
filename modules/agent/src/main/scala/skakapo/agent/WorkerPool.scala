package skakapo.agent

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import skakapo.core.ErrorCode
import skakapo.db.*
import sbirds.observe.{Log, Metrics}
import skakapo.shared.RunStatus

import java.time.Instant
import scala.concurrent.duration.*

/** node はこのノードの名前、slots は同時に動かせる実行の数. */
final case class PoolConfig(
  node: String,
  version: String,
  slots: Int = 2,
  leaseSeconds: Int = 90)

/**
 * 実行の取り合い（冗長構成）. どのノードも DB の実行の行列を見て、空きがあれば1つずつ取る.
 *  - 空いているノードが先に取る: ほかに、使っている割合がこのノードより小さいノードがあれば、少しの間は取らずに任せる
 *    （ただし 10 秒以上待っている実行があれば、空きのあるノードならどこでも取る）
 *  - 取った実行は借り（lease）を 30 秒ごとに延ばす. ノードが止まって借りが切れると、ほかのノードが続きから引き継ぐ（3回まで）
 *  - 利用者が止めたら、借りを延ばすときに分かるので、次のターン・ツールの前で止める
 */
final class WorkerPool(
  sessions: SessionStore,
  nodes: NodeStore,
  runner: Runner,
  cfg: PoolConfig) {

  private val active = Ref.unsafe[IO, Int](0)

  def run: Resource[IO, Unit] =
    for {
      started <- Resource.eval(IO.realTimeInstant)
      _ <- Resource.make(IO.unit)(_ => nodes.leave(cfg.node).handleError(_ => ()))
      _ <- heartbeat(started).background
      _ <- loop.background
      _ <-
      (IO.sleep(1.minute) *> sessions.failAbandoned.flatMap(_.traverse_(id =>
        sessions.event(id, "error", Json.obj("text" -> "ノードが続けて止まったため、作業を終えられませんでした。もう一度返事をしてください。".asJson))
      ))
        .handleError(_ => List()).void).foreverM.background
    } yield ()

  private def heartbeat(
    started: Instant
  ): IO[Unit] =
    (active.get.flatMap(n => nodes.heartbeat(cfg.node, cfg.version, cfg.slots, n, started)).handleErrorWith(e =>
      Log.report(ErrorCode.HeartbeatFailed, e.toString, e)
    ) *> IO.sleep(10.seconds)).foreverM

  private def loop: IO[Unit] =
    (tick.handleErrorWith(e => Log.report(ErrorCode.WorkerLoopFailed, e.toString, e)) *> IO.sleep(2.seconds)).foreverM

  /** 空きがあれば、実行を取って裏で動かす（取れるだけ取る）. */
  private def tick: IO[Unit] =
    active.get.flatMap { n =>
      if n >= cfg.slots then IO.unit
      else {
        // 待っている実行がなければ、ほかのノードの空きは調べない
        val yieldToIdler = sessions.sweepAndHasQueued.flatMap {
          case false => IO.pure(None)
          case true => (nodes.someoneIdler(cfg.node, n, cfg.slots), nodes.oldestWaitSeconds).mapN(
              (
                idler,
                wait
              ) => Some(idler && wait.forall(_ < 10))
            )
        }
        yieldToIdler.flatMap {
          case None | Some(true) => IO.unit
          case Some(false) =>
            active.modify(c => if c < cfg.slots then (c + 1, true) else (c, false)).flatMap {
              case false => IO.unit
              case true =>
                sessions.claim(cfg.node, cfg.leaseSeconds).flatMap {
                  case None => active.update(_ - 1)
                  case Some(r) =>
                    (Metrics.upDown("skakapo.runs.active", 1) *> execute(r)
                      .guarantee(active.update(_ - 1) *> Metrics.upDown("skakapo.runs.active", -1))).start *> tick
                }
            }
        }
      }
    }

  private def execute(
    r: Run
  ): IO[Unit] = {
    // 引き継いだ実行に取り消しが頼まれていれば、最初から止める
    val cancelled = Ref.unsafe[IO, Boolean](r.cancelRequested)
    val lost = Ref.unsafe[IO, Boolean](false)
    // 借りを延ばす（取り消しの合図・ほかのノードに移ったことも、ここで分かる）
    val renew =
      (IO.sleep((cfg.leaseSeconds / 3).seconds) *> sessions.renew(r.id, cfg.node, cfg.leaseSeconds).flatMap {
        case Some(c) => cancelled.set(c)
        case None => lost.set(true) *> cancelled.set(true)
      }.handleError(_ => ())).foreverM
    IO.realTime.flatMap { t0 =>
      renew.background.use { _ =>
        runner.run(r, cancelled.get, cfg.node).handleErrorWith(e =>
          Log.report(ErrorCode.RunFailed, s"run ${r.id}: $e", e) *>
            IO.pure((RunStatus.Failed, Some(s"内部のエラー（${e.getClass.getSimpleName}）")))
        )
      }.flatMap {
        (
          status,
          error
        ) =>
          lost.get.flatMap {
            // ほかのノードに移った（借りが切れた）なら、そちらに任せる
            case true => Log.println(s"[skakapo] run ${r.id} moved to another node")
            case false =>
              for {
                t1 <- IO.realTime
                mine <- sessions.finish(r.id, r.sessionId, cfg.node, status, error)
                _ <- Metrics.count("skakapo.runs.finished", "status" -> status)
                _ <-
                  Metrics.record("skakapo.runs.duration", (t1 - t0).toMillis.toDouble / 1000, "s", "status" -> status)
                // 持っていなかった（取り消された・ほかのノードに移った）なら、通知はそちらに任せる
                _ <- IO.whenA(mine)(sessions.find(r.sessionId).flatMap(_.traverse_(runner.notifyDone(_, status))))
              } yield ()
          }
      }
    }
  }
}

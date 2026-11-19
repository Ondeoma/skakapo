package skakapo.agent

import cats.effect.*

import scala.concurrent.duration.*

/**
 * 期限つきの値（短い期限のトークン・ツールの一覧など）を覚える. 期限の margin 前までは使い回し、それより近ければ作り直す
 * （1000件を超えたら全部忘れる）.
 */
final class TokenCache[K, V](
  margin: FiniteDuration = 5.minutes) {
  private val ref = Ref.unsafe[IO, Map[K, (Long, V)]](Map.empty)

  /** 覚えていればそれを、なければ issue で発行して覚える（issue は値と期限（ミリ秒）を返す）. */
  def get(
    key: K
  )(
    issue: IO[Either[String, (V, Long)]]
  ): IO[Either[String, V]] =
    IO.realTime.map(_.toMillis).flatMap { now =>
      ref.get.map(_.get(key).filter(_._1 - margin.toMillis > now)).flatMap {
        case Some((_, v)) => IO.pure(Right(v))
        case None => issue.flatMap {
            case Right((v, exp)) =>
              ref.update(m => (if m.size > 1000 then Map.empty else m) + (key -> (exp, v))).as(Right(v))
            case Left(e) => IO.pure(Left(e))
          }
      }
    }

  /** 当てはまるものを忘れる（秘密を変えた・消したとき）. */
  def forget(
    p: K => Boolean
  ): IO[Unit] = ref.update(_.filterNot(
    (
      k,
      _
    ) => p(k)
  ))
}

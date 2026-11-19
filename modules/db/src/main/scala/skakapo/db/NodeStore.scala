package skakapo.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skakapo.shared.*

import java.time.Instant

/** 動いているノード（心拍）と、全体の様子. */
final class NodeStore(
  xa: Transactor[IO]) extends TxStore(xa) {

  def heartbeat(
    name: String,
    version: String,
    slots: Int,
    active: Int,
    startedAt: Instant
  ): IO[Unit] = tx(
    sql"""insert into node (name, version, slots, active, started_at, seen_at) values ($name, $version, $slots, $active, $startedAt, now())
          on conflict (name) do update set version = excluded.version, slots = excluded.slots, active = excluded.active,
            started_at = excluded.started_at, seen_at = now()""".update.run.void
  )

  /** 止めるときに心拍を消す（ほかのノードの「空いているか」の判断から外す）. */
  def leave(
    name: String
  ): IO[Unit] = tx(sql"delete from node where name = $name".update.run.void)

  /**
   * ほかに、このノードより空いている（使っている割合が小さい）動いているノードがあるか.
   * あれば、待っている実行はそちらに任せる（空いているマシンで作業する）.
   */
  def someoneIdler(
    name: String,
    active: Int,
    slots: Int
  ): IO[Boolean] = tx(
    (fr"""select exists (select 1 from node where name <> $name and active < slots and seen_at >""" ++
      Sql.AliveWindow ++
      fr"""and active::float / slots < ${active.toDouble / slots})""").query[Boolean].unique
  )

  /** 一番古い待っている実行が、どれだけ待っているか（秒）. */
  def oldestWaitSeconds: IO[Option[Double]] = tx(
    sql"select extract(epoch from now() - min(created_at))::float8 from run where status = 'queued'".query[Option[
      Double
    ]].unique
  )

  def stats: IO[StatsDto] = tx(
    for {
      nodes <-
      (fr"select name, version, slots, active, started_at, seen_at, seen_at >" ++ Sql.AliveWindow ++
        fr"from node order by name").query[(String, String, Int, Int, Instant, Instant, Boolean)].to[List]
      q <-
        sql"select count(*) filter (where status = 'queued'), count(*) filter (where status = 'running') from run where status in ('queued', 'running')"
          .query[(Int, Int)].unique
      d <-
        sql"select count(*)::int, coalesce(sum(input_tokens + output_tokens), 0)::bigint from run where created_at > now() - interval '24 hours'"
          .query[(Int, Long)].unique
    } yield StatsDto(
      nodes.map(n => NodeDto(n._1, n._2, n._3, n._4, n._5.toString, n._6.toString, n._7)),
      q._1,
      q._2,
      d._1,
      d._2
    )
  )
}

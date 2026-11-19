package skakapo.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.util.UUID

/** クレジット（Claude API の費用. ドル）: 今月の使った額と、利用者ごとの上限. 月の区切りは UTC. */
final class CreditStore(xa: Transactor[IO]) extends TxStore(xa) {

  /** 今月（UTC）の台帳の行（別名 c）. 会話を消しても台帳は残るので、上限を戻せない. */
  private val thisMonth = fr"c.created_at >= date_trunc('month', now() at time zone 'UTC') at time zone 'UTC'"

  private val monthLabel = fr"to_char(now() at time zone 'UTC', 'YYYY-MM')"

  /** (月, 全体の今月の使った額). */
  def totals: IO[(String, Double)] =
    tx((fr"select" ++ monthLabel ++ fr", coalesce(sum(c.cost_usd), 0)::float8 from credit_usage c where" ++ thisMonth)
      .query[(String, Double)].unique)

  /** 1回の問い合わせで: (月, この人の今月の使った額, 全体の今月の使った額, 人ごとの上限（None は既定のまま. Some(None) は上限なし）). */
  def summary(userId: UUID): IO[(String, Double, Double, Option[Option[Double]])] = tx(
    (fr"select" ++ monthLabel ++
      fr""",
                 coalesce(sum(c.cost_usd) filter (where c.user_id = $userId), 0)::float8, coalesce(sum(c.cost_usd), 0)::float8,
                 exists (select 1 from user_budget where user_id = $userId), (select cap_usd::float8 from user_budget where user_id = $userId)
          from credit_usage c where""" ++ thisMonth)
      .query[(String, Double, Double, Boolean, Option[Double])].unique.map((m, mine, total, has, cap) =>
        (m, mine, total, Option.when(has)(cap))
      )
  )

  def setCap(userId: UUID, cap: Option[Double], by: UUID): IO[Unit] = tx(
    sql"""insert into user_budget (user_id, cap_usd, updated_by) values ($userId, $cap, $by)
          on conflict (user_id) do update set cap_usd = excluded.cap_usd, updated_by = excluded.updated_by, updated_at = now()""".update.run.void
  )

  def clearCap(userId: UUID): IO[Unit] = tx(sql"delete from user_budget where user_id = $userId".update.run.void)

  /** 利用者ごとの今月の使った額・実行の数・人ごとの上限（使った額の多い順. 今月使っていない人と、上限を変えた人も含める）. */
  def perUser: IO[List[(User, Double, Int, Option[Option[Double]])]] = tx {
    (fr"select" ++ UserCols.cols("u") ++
      fr""", coalesce(m.used, 0)::float8, coalesce(m.runs, 0)::int, b.user_id is not null, b.cap_usd::float8
         from app_user u
         left join (select c.user_id, sum(c.cost_usd) as used, count(distinct c.run_id) as runs from credit_usage c
                    where""" ++ thisMonth ++ fr"""group by c.user_id) m on m.user_id = u.id
         left join user_budget b on b.user_id = u.id
         where not u.disabled
         order by coalesce(m.used, 0) desc, u.name limit 1000""")
      .query[(User, Double, Int, Boolean, Option[Double])].to[List]
      .map(_.map((u, used, runs, has, cap) => (u, used, runs, Option.when(has)(cap))))
  }
}

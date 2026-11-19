package skakapo.agent

import cats.effect.*
import skakapo.db.{ConfigStore, CreditStore}
import skakapo.shared.*

import java.util.UUID

/** クレジット（Claude API の費用. ドル）の残り. 全体の月の予算と、利用者ごとの月の上限（人ごとの上限、なければ既定）. */
final class Credits(store: CreditStore, config: ConfigStore) {

  def of(userId: UUID): IO[CreditDto] = config.agent.flatMap(of(userId, _))

  /** 設定を持っているとき（実行の各ターン）は、設定を読み直さない. */
  def of(userId: UUID, a: AgentSettings): IO[CreditDto] =
    store.summary(userId).map {
      (month, mine, total, ov) =>
        {
          val cap = ov.getOrElse(a.defaultUserCapUsd)
          CreditDto(
            month,
            mine,
            cap,
            cap.map(c => math.max(c - mine, 0)),
            total,
            a.monthlyBudgetUsd,
            a.monthlyBudgetUsd.map(b => math.max(b - total, 0))
          )
        }
    }

  /** 使えなくなった理由（使えるなら None）. */
  def blocked(userId: UUID, a: AgentSettings): IO[Option[String]] = of(userId, a).map(_.blocked)
}

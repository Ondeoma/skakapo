package skakapo.db

import doobie.*
import doobie.implicits.*

/** ノードの生死を見る SQL の部品（skakapo 固有. 共通の部品は sbirds.db.Sql）. */
object NodeSql {

  /** ノードが止まったとみなすまでの、心拍のない時間. */
  val AliveWindow: Fragment = fr"now() - interval '45 seconds'"
}

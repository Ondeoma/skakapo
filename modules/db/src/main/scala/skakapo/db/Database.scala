package skakapo.db

import cats.effect.*
import com.zaxxer.hikari.HikariConfig
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import org.flywaydb.core.Flyway
import skakapo.core.ErrorCode

import scala.concurrent.duration.FiniteDuration

final case class DbConfig(url: String, user: String, password: String, maxPoolSize: Int = 10)

object Database {

  def migrate(cfg: DbConfig): IO[Int] =
    IO.blocking(Flyway.configure().dataSource(cfg.url, cfg.user, cfg.password).load().migrate().migrationsExecuted)

  def transactor(cfg: DbConfig): Resource[IO, HikariTransactor[IO]] = {
    val hc = new HikariConfig()
    hc.setJdbcUrl(cfg.url)
    hc.setUsername(cfg.user)
    hc.setPassword(cfg.password)
    hc.setMaximumPoolSize(cfg.maxPoolSize)
    hc.setDriverClassName("org.postgresql.Driver")
    hc.setConnectionTimeout(5000)
    hc.setValidationTimeout(3000)
    hc.setKeepaliveTime(60000)
    hc.setMaxLifetime(600000)
    // SQL ごとにトレースの子のスパンと所要時間のメトリクスを残す（skorimako）
    HikariTransactor.fromHikariConfig[IO](hc, Some(sbirds.observe.DbTelemetry.logHandler))
  }
}

/** 定期的に動かす処理. */
object Periodic {

  /** initialDelay 待ってから action を動かし、以後 every ごとに繰り返す. 失敗しても止めず、code を付けてログに出す. */
  def run(initialDelay: FiniteDuration, every: FiniteDuration, code: ErrorCode)(action: IO[Any]): IO[Nothing] =
    IO.sleep(initialDelay) *>
      (action.void.handleErrorWith(e => sbirds.observe.Log.report(code, e.toString, e)) *> IO.sleep(every)).foreverM
}

/** 各メソッドを1トランザクションで実行するストアの共通部分. */
abstract class TxStore(xa: Transactor[IO]) {
  protected def tx[A](c: ConnectionIO[A]): IO[A] = c.transact(xa)
}

/** SQL の部品. */
object Sql {

  /** set a = 1, b = 2（空のリストは渡さないこと）. */
  def set(fs: List[Fragment]): Fragment = fr"set" ++ fs.reduce(_ ++ fr"," ++ _)

  /** ノードが止まったとみなすまでの、心拍のない時間. */
  val AliveWindow: Fragment = fr"now() - interval '45 seconds'"

  /** 部分一致の LIKE のパターン（% と _ はそのままの文字として探す）. */
  def like(q: String): String = "%" + q.trim.replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_") + "%"
}

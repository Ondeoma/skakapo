package skakapo.db

import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.time.Instant
import java.util.UUID

/** 利用者が連携したサービスのトークン（token・refresh は暗号化した値）. */
final case class Connection(userId: UUID,
                            serviceId: UUID,
                            token: String,
                            refresh: String,
                            scope: String,
                            via: String,
                            expiresAt: Option[Instant],
                            connectedAt: Instant)

/** 利用者ごとの連携と、OAuth の連携の途中の状態. */
final class ConnectionStore(xa: Transactor[IO]) extends TxStore(xa) {

  private val cols = fr"user_id, service_id, token, refresh, scope, via, expires_at, connected_at"

  def list(userId: UUID): IO[List[Connection]] =
    tx((fr"select" ++ cols ++ fr"from connection where user_id = $userId").query[Connection].to[List])

  def find(userId: UUID, serviceId: UUID): IO[Option[Connection]] = tx((fr"select" ++ cols ++
    fr"from connection where user_id = $userId and service_id = $serviceId").query[Connection].option)

  def save(c: Connection): IO[Unit] = tx(
    sql"""insert into connection (user_id, service_id, token, refresh, scope, via, expires_at)
          values (${c.userId}, ${c.serviceId}, ${c.token}, ${c.refresh}, ${c.scope}, ${c.via}, ${c.expiresAt})
          on conflict (user_id, service_id) do update set token = excluded.token, refresh = excluded.refresh, scope = excluded.scope,
            via = excluded.via, expires_at = excluded.expires_at, connected_at = now()""".update.run.void
  )

  /** サービスの連携を全員分外す. */
  def deleteForService(serviceId: UUID): IO[Unit] =
    tx(sql"delete from connection where service_id = $serviceId".update.run.void)

  def delete(userId: UUID, serviceId: UUID): IO[Option[Connection]] =
    tx((fr"delete from connection where user_id = $userId and service_id = $serviceId returning" ++ cols).query[
      Connection
    ].option)

  // ---------------------------------------------------------------- OAuth の途中の状態

  def saveState(state: String, userId: UUID, serviceId: UUID, verifier: String, returnTo: String): IO[Unit] = tx(
    sql"insert into oauth_state (state, user_id, service_id, verifier, return_to) values ($state, $userId, $serviceId, $verifier, $returnTo)".update.run.void
  )

  /** 取り出して消す（1回だけ・15分以内）. (利用者, サービス, verifier, 戻る先). */
  def takeState(state: String): IO[Option[(UUID, UUID, String, String)]] = tx(
    sql"""delete from oauth_state where state = $state and created_at > now() - interval '15 minutes'
          returning user_id, service_id, verifier, return_to""".query[(UUID, UUID, String, String)].option
  )

  def purge: IO[Unit] = tx(sql"delete from oauth_state where created_at < now() - interval '1 hour'".update.run.void)
}

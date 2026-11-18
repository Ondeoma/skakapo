package skakapo.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skakapo.shared.*

import java.time.Instant
import java.util.UUID

/** avatarUrl は表示するアイコン（保存したものがあればその URL、なければ IdP の画像）. */
final case class User(id: UUID, sub: String, name: String, email: Option[String], avatarUrl: Option[String], timezone: String, disabled: Boolean,
    avatarUploaded: Boolean, isAdmin: Boolean, showDetails: Boolean = false, idpGroups: List[String] = Nil, groupId: Option[UUID] = None):
  def me: MeDto        = MeDto(id, name, email, avatarUrl, timezone, avatarUploaded, isAdmin, showDetails)
  def brief: UserBrief = me.brief

/** app_user の列（別名 a）. */
object UserCols:
  /** 表示するアイコンの URL. 保存したアイコンがあれば /api/users/{id}/avatar?v={版}（版を変えるとブラウザのキャッシュが切り替わる）. */
  def avatar(a: String): Fragment = Fragment.const(
    s"(case when $a.avatar_version is not null then '/api/users/' || $a.id || '/avatar?v=' || $a.avatar_version else $a.avatar_url end)")

  /** User を読む列（a はテーブルの別名）. */
  def cols(a: String): Fragment =
    Fragment.const(s"$a.id, $a.sub, $a.name, $a.email,") ++ avatar(a) ++ Fragment.const(s", $a.timezone, $a.disabled, $a.avatar_version is not null, $a.is_admin, $a.show_details, $a.idp_groups, $a.group_id")

/** ユーザー・セッション・ログイン途中の状態. */
final class UserStore(xa: Transactor[IO]) extends TxStore(xa):

  private val cols = UserCols.cols("app_user")

  /** ログインのたびに作成・更新する（名前・メール・アイコン・管理者かどうかは IdP の値に合わせる）. */
  /** ログインしたとき: 作るか、名前・メール・画像・管理者か・IdP のグループを合わせる. */
  def upsert(sub: String, name: String, email: Option[String], avatar: Option[String], admin: Boolean, groups: List[String] = Nil): IO[User] = tx(
    (fr"""insert into app_user (id, sub, name, email, avatar_url, is_admin, idp_groups, last_login_at)
          values (${UUID.randomUUID()}, $sub, $name, $email, $avatar, $admin, $groups, now())
          on conflict (sub) do update set name = excluded.name, email = excluded.email,
            avatar_url = excluded.avatar_url, is_admin = excluded.is_admin, idp_groups = excluded.idp_groups, last_login_at = now()
          returning""" ++ cols).query[User].unique
  )

  def find(id: UUID): IO[Option[User]] = tx(findC(id))

  /** 管理画面の利用者の一覧（無効な人を除く. 名前の順）. */
  def listActive: IO[List[User]] = tx((fr"select" ++ cols ++ fr"from app_user where not disabled order by name limit 2000").query[User].to[List])

  /** 管理者がその人に種類を付ける（None なら外す）. */
  def assignGroup(id: UUID, groupId: Option[UUID]): IO[Boolean] = tx(sql"update app_user set group_id = $groupId where id = $id".update.run.map(_ > 0))

  def findC(id: UUID): ConnectionIO[Option[User]] = (fr"select" ++ cols ++ fr"from app_user where id = $id").query[User].option

  /** メールアドレス・ID で1人を決める（共有の相手の指定）. */
  def resolve(s: String): IO[Option[User]] = tx {
    val t = s.trim
    scala.util.Try(UUID.fromString(t)).toOption match
      case Some(id) => findC(id)
      case None => (fr"select" ++ cols ++ fr"from app_user where not disabled and lower(email) = lower($t) order by last_login_at desc nulls last limit 1")
          .query[User].option
  }

  def updateMe(id: UUID, name: Option[String], timezone: Option[String], showDetails: Option[Boolean] = None): IO[Unit] = tx {
    val sets = List(name.map(_.trim).filter(_.nonEmpty).map(v => fr"name = $v"), timezone.map(v => fr"timezone = $v"),
      showDetails.map(v => fr"show_details = $v")).flatten
    if sets.isEmpty then ().pure[ConnectionIO] else (fr"update app_user" ++ Sql.set(sets) ++ fr"where id = $id").update.run.void
  }

  // ---------------------------------------------------------------- アイコン

  /** アイコンを保存する（版を1つ進める）. */
  def saveAvatar(id: UUID, contentType: String, data: Array[Byte]): IO[Unit] = tx(
    sql"""insert into user_avatar (user_id, content_type, data) values ($id, $contentType, $data)
          on conflict (user_id) do update set content_type = excluded.content_type, data = excluded.data, updated_at = now()""".update.run *>
      sql"update app_user set avatar_version = coalesce(avatar_version, 0) + 1 where id = $id".update.run.void
  )

  /** 保存したアイコンを消す（IdP の画像に戻る）. */
  def deleteAvatar(id: UUID): IO[Unit] = tx(
    sql"delete from user_avatar where user_id = $id".update.run *> sql"update app_user set avatar_version = null where id = $id".update.run.void
  )

  def avatar(id: UUID): IO[Option[(String, Array[Byte])]] = tx(
    sql"select content_type, data from user_avatar where user_id = $id".query[(String, Array[Byte])].option
  )

  // ---------------------------------------------------------------- セッション

  def createSession(hash: Array[Byte], userId: UUID, idToken: Option[String], expires: Instant): IO[Unit] = tx(
    sql"insert into session (token_hash, user_id, id_token, expires_at) values ($hash, $userId, $idToken, $expires)".update.run.void
  )

  def findSession(hash: Array[Byte], now: Instant): IO[Option[User]] = tx(
    sql"select user_id from session where token_hash = $hash and expires_at > $now".query[UUID].option.flatMap {
      case None      => none[User].pure[ConnectionIO]
      case Some(uid) => findC(uid).map(_.filterNot(_.disabled))
    }
  )

  /** セッションを消し、IdP のログアウトに渡す ID トークンを返す. */
  def deleteSession(hash: Array[Byte]): IO[Option[String]] = tx(
    sql"delete from session where token_hash = $hash returning id_token".query[Option[String]].option.map(_.flatten)
  )

  def purgeExpired(now: Instant): IO[Unit] = tx(
    sql"delete from session where expires_at < $now".update.run *>
      sql"delete from login_state where created_at < now() - interval '1 hour'".update.run.void
  )

  // ---------------------------------------------------------------- API トークン

  private val tokenCols = fr"id, name, prefix, scope, created_at, expires_at, last_used_at"

  private def tokenDto(t: (UUID, String, String, String, Instant, Option[Instant], Option[Instant])): ApiTokenDto =
    ApiTokenDto(t._1, t._2, t._3, t._4, t._5.toString, t._6.map(_.toString), t._7.map(_.toString))

  def createToken(userId: UUID, name: String, prefix: String, hash: Array[Byte], scope: String, expires: Option[Instant]): IO[ApiTokenDto] = tx(
    (fr"insert into api_token (id, user_id, name, prefix, token_hash, scope, expires_at) values (${UUID.randomUUID()}, $userId, $name, $prefix, $hash, $scope, $expires) returning" ++ tokenCols)
      .query[(UUID, String, String, String, Instant, Option[Instant], Option[Instant])].unique.map(tokenDto)
  )

  /** 失効していないもの（期限切れも含む. 画面で期限切れと分かるように）. */
  def tokens(userId: UUID): IO[List[ApiTokenDto]] = tx(
    (fr"select" ++ tokenCols ++ fr"from api_token where user_id = $userId and revoked_at is null order by created_at desc")
      .query[(UUID, String, String, String, Instant, Option[Instant], Option[Instant])].to[List].map(_.map(tokenDto))
  )

  def revokeToken(userId: UUID, id: UUID): IO[Boolean] = tx(
    sql"update api_token set revoked_at = now() where id = $id and user_id = $userId and revoked_at is null".update.run.map(_ > 0)
  )

  /**
   * 有効なトークンの持ち主とスコープ. 最後に使った日時は1分に1回だけ書く.
   * 持ち主が30日ログインしていなければ使えない（IdP のグループから外れた人が、トークンで使い続けないように）.
   */
  def findToken(hash: Array[Byte], now: Instant): IO[Option[(User, UUID, String)]] = tx(
    sql"""select id, user_id, scope from api_token
          where token_hash = $hash and revoked_at is null and (expires_at is null or expires_at > $now)"""
      .query[(UUID, UUID, String)].option.flatMap {
        case None => none[(User, UUID, String)].pure[ConnectionIO]
        case Some((id, uid, scope)) =>
          sql"update api_token set last_used_at = $now where id = $id and (last_used_at is null or last_used_at < $now - interval '1 minute')".update.run *>
            sql"select coalesce(last_login_at > now() - interval '30 days', false) from app_user where id = $uid".query[Boolean].option.flatMap {
              case Some(true) => findC(uid).map(_.filterNot(_.disabled).map(u => (u, id, scope)))
              case _          => none[(User, UUID, String)].pure[ConnectionIO]
            }
      }
  )

  // ---------------------------------------------------------------- ログイン途中の状態（OIDC）

  def saveLoginState(state: String, verifier: String, nonce: String, returnTo: String): IO[Unit] = tx(
    sql"insert into login_state (state, verifier, nonce, return_to) values ($state, $verifier, $nonce, $returnTo)".update.run.void
  )

  /** 取り出して消す（1回だけ・15分以内）. */
  def takeLoginState(state: String): IO[Option[(String, String, String)]] = tx(
    sql"""delete from login_state where state = $state and created_at > now() - interval '15 minutes'
          returning verifier, nonce, return_to""".query[(String, String, String)].option
  )

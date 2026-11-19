package skakapo.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import doobie.postgres.implicits.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import skakapo.core.Defaults
import skakapo.shared.*

import java.time.Instant
import java.util.UUID

/** 管理画面の設定（全体の設定・連携するサービスとそのツール・リポジトリ・依頼の種類）. */
final class ConfigStore(xa: Transactor[IO]) extends TxStore(xa) {

  // ---------------------------------------------------------------- 全体の設定（key ごとの JSON）

  def setting[A: Decoder](key: String, default: A): IO[A] =
    tx(sql"select value from setting where key = $key".query[Json].option.map(_.flatMap(
      _.as[A].toOption
    ).getOrElse(default)))

  def saveSetting[A: Encoder](key: String, value: A, by: UUID): IO[Unit] = tx(
    sql"""insert into setting (key, value, updated_by) values ($key, ${value.asJson}, $by)
          on conflict (key) do update set value = excluded.value, updated_by = excluded.updated_by, updated_at = now()""".update.run.void
  )

  def agent: IO[AgentSettings] = setting("agent", Defaults.agent)

  /** 起票先（複数）. 古い1つだけの設定（tracker）があれば、それを既定の1つとして読む. */
  def trackers: IO[List[TrackerSettings]] =
    setting[Option[List[TrackerSettings]]]("trackers", None).flatMap {
      case Some(ts) if ts.nonEmpty => IO.pure(ts)
      case _ => setting[Option[TrackerSettings]]("tracker", None).map(_.map(t =>
          List(t.copy(isDefault = true))
        ).getOrElse(Defaults.trackers))
    }

  // ---------------------------------------------------------------- 利用者が登録する前提

  def userContext(userId: UUID): IO[String] =
    tx(sql"select text from user_context where user_id = $userId".query[String].option.map(_.getOrElse("")))

  def saveUserContext(userId: UUID, text: String): IO[Unit] = tx(
    sql"""insert into user_context (user_id, text) values ($userId, $text)
          on conflict (user_id) do update set text = excluded.text, updated_at = now()""".update.run.void
  )

  // ---------------------------------------------------------------- サービス

  private val serviceCols =
    fr"""s.id, s.key, s.name, s.kind, s.base_url, s.mcp_url, s.description, s.notes, s.oauth, s.oauth_secret <> '', s.manual_token, s.enabled, s.position,
      s.agent_token <> '', s.agent_account"""

  private type ServiceRow = (
    UUID,
    String,
    String,
    String,
    String,
    String,
    String,
    String,
    Json,
    Boolean,
    Boolean,
    Boolean,
    Int,
    Boolean,
    String
  )

  private def serviceDto(r: ServiceRow): ServiceDto =
    ServiceDto(
      r._1,
      r._2,
      r._3,
      r._4,
      r._5,
      r._6,
      r._7,
      r._8,
      r._9.as[OAuthConfig].getOrElse(OAuthConfig()),
      r._10,
      r._11,
      r._12,
      r._13,
      hasAgentToken = r._14,
      agentAccount = r._15
    )

  /** サービスに、覚えているツールを付ける. */
  private def withTools(ss: List[ServiceDto]): ConnectionIO[List[ServiceDto]] =
    toolsC(ss.map(_.id)).map(ts =>
      ss.map(s =>
        ts.get(s.id).fold(s)((at, tools) => s.copy(tools = tools, toolsUpdatedAt = Some(at.toString)))
      )
    )

  /** サービスの一覧（覚えているツールも付ける）. enabledOnly なら有効なものだけ. */
  def services(enabledOnly: Boolean = false): IO[List[ServiceDto]] = tx((fr"select" ++ serviceCols ++
    fr"from service s" ++ (if enabledOnly then fr"where s.enabled" else Fragment.empty) ++
    fr"order by s.position, s.name").query[ServiceRow].to[List].map(_.map(serviceDto)).flatMap(withTools))

  def service(id: UUID): IO[Option[ServiceDto]] =
    tx((fr"select" ++ serviceCols ++ fr"from service s where s.id = $id").query[ServiceRow].option.map(_.map(
      serviceDto
    ).toList).flatMap(withTools).map(_.headOption))

  /** 暗号化した OAuth のクライアントのシークレット. */
  def serviceSecret(id: UUID): IO[Option[String]] =
    tx(sql"select oauth_secret from service where id = $id".query[String].option.map(_.filter(_.nonEmpty)))

  /** 有効なサービスのエージェント専用アカウントのトークン（暗号化した値）. */
  def agentTokens: IO[Map[UUID, String]] =
    tx(sql"select id, agent_token from service where enabled and agent_token <> ''".query[(
      UUID,
      String
    )].to[List].map(_.toMap))

  /** secret・agentToken: None は変えない、Some("") は消す、Some(暗号化した値) は置き換える. */
  def saveService(id: Option[UUID],
                  r: ServiceReq,
                  secret: Option[String],
                  agentToken: Option[String] = None): IO[Either[String, UUID]] = tx {
    val sid = id.getOrElse(UUID.randomUUID())
    val upsert = id match {
      case None =>
        sql"""insert into service (id, key, name, kind, base_url, mcp_url, description, notes, oauth, oauth_secret, manual_token, enabled, position,
                                   agent_account, agent_token)
              values ($sid, ${r.key}, ${r.name}, ${r.kind}, ${r.baseUrl}, ${r.mcpUrl}, ${r.description}, ${r.notes}, ${r.oauth.asJson},
                      ${secret.getOrElse(
            ""
          )}, ${r.manualToken}, ${r.enabled}, ${r.position}, ${r.agentAccount}, ${agentToken.getOrElse("")})
              on conflict (key) do nothing""".update.run
      case Some(_) =>
        (fr"""update service set key = ${r.key}, name = ${r.name}, kind = ${r.kind}, base_url = ${r.baseUrl}, mcp_url = ${r.mcpUrl},
              description = ${r.description}, notes = ${r.notes}, oauth = ${r.oauth.asJson}, manual_token = ${r.manualToken},
              enabled = ${r.enabled}, position = ${r.position}, agent_account = ${r.agentAccount}, updated_at = now()""" ++
          secret.fold(Fragment.empty)(s => fr", oauth_secret = $s") ++
          agentToken.fold(Fragment.empty)(t => fr", agent_token = $t") ++
          fr"where id = $sid and not exists (select 1 from service o where o.key = ${r.key} and o.id <> $sid)")
          .update.run
    }
    upsert.map(n =>
      Either.cond(n > 0, sid, if id.isEmpty then s"キー「${r.key}」のサービスがすでにあります" else s"キー「${r.key}」のサービスがすでにあるか、見つかりません")
    )
  }

  def deleteService(id: UUID): IO[Boolean] = tx(sql"delete from service where id = $id".update.run.map(_ > 0))

  // ---------------------------------------------------------------- サービスのツール（システムとして何ができるかを覚えておく）

  private def toolsC(ids: List[UUID]): ConnectionIO[Map[UUID, (Instant, List[ToolInfo])]] =
    if ids.isEmpty then Map.empty.pure[ConnectionIO]
    else
      sql"select service_id, name, description, read_only, updated_at from service_tool where service_id = any(${ids.toArray}) order by name"
        .query[(UUID, String, String, Option[Boolean], Instant)].to[List]
        .map(_.groupBy(_._1).map((id, rows) => id -> (rows.map(_._5).max, rows.map(r => ToolInfo(r._2, r._3, r._4)))))

  /** tools/list の結果で置き換える（消えたツールは消す）. */
  def replaceTools(serviceId: UUID, tools: List[(ToolInfo, Json)]): IO[Unit] =
    tx(sql"delete from service_tool where service_id = $serviceId".update.run *>
      Update[(UUID, String, String, Json, Option[Boolean])](
        "insert into service_tool (service_id, name, description, input_schema, read_only) values (?, ?, ?, ?, ?) on conflict do nothing"
      ).updateMany(tools.map((t, schema) => (serviceId, t.name, t.description, schema, t.readOnly))).void)

  // ---------------------------------------------------------------- リポジトリ

  private val repoCols = fr"id, name, url, default_branch, service_key, description, notes, enabled, account_id"

  def repositories(enabledOnly: Boolean = false): IO[List[RepositoryDto]] =
    tx((fr"select" ++ repoCols ++ fr"from repository" ++ (if enabledOnly then fr"where enabled" else Fragment.empty) ++
      fr"order by name").query[RepositoryDto].to[List])

  /** 有効なサービスのキーと、有効なリポジトリの名前（フォームの選択の確かめ. ツールまでは読まない）. */
  def enabledRefs: IO[(Set[String], Set[String])] = tx((
    sql"select key from service where enabled".query[String].to[List],
    sql"select name from repository where enabled".query[String].to[List]
  ).mapN((a, b) => (a.toSet, b.toSet)))

  def saveRepository(id: Option[UUID], r: RepositoryReq): IO[Either[String, RepositoryDto]] = tx {
    val rid = id.getOrElse(UUID.randomUUID())
    val q = id match {
      case None =>
        (fr"""insert into repository (id, name, url, default_branch, service_key, description, notes, enabled, account_id)
              values ($rid, ${r.name}, ${r.url}, ${r.defaultBranch}, ${r.serviceKey}, ${r.description}, ${r.notes}, ${r.enabled}, ${r.accountId})
              on conflict (name) do nothing returning""" ++ repoCols).query[RepositoryDto].option
      case Some(_) =>
        (fr"""update repository set name = ${r.name}, url = ${r.url}, default_branch = ${r.defaultBranch}, service_key = ${r.serviceKey},
              description = ${r.description}, notes = ${r.notes}, enabled = ${r.enabled}, account_id = ${r.accountId}, updated_at = now()
              where id = $rid and not exists (select 1 from repository o where o.name = ${r.name} and o.id <> $rid) returning""" ++
          repoCols)
          .query[RepositoryDto].option
    }
    q.map(_.toRight(s"名前「${r.name}」のリポジトリがすでにあるか、見つかりません"))
  }

  def deleteRepository(id: UUID): IO[Boolean] = tx(sql"delete from repository where id = $id".update.run.map(_ > 0))

  // ---------------------------------------------------------------- Git の共有アカウント（トークンは暗号化した値）

  private val accountCols =
    fr"""id, name, provider, api_url, username, token <> '', author_name, author_email, auth_kind, app_id, installation_id"""

  def gitAccounts: IO[List[GitAccountDto]] =
    tx((fr"select" ++ accountCols ++ fr"from git_account order by name").query[GitAccountDto].to[List])

  /** 暗号化したトークンも返す（実行のときだけ使う）. */
  def gitAccountWithToken(id: UUID): IO[Option[(GitAccountDto, String)]] =
    tx((fr"select" ++ accountCols ++ fr", token from git_account where id = $id").query[(GitAccountDto, String)].option)

  /** token: None は変えない（新しく作るときは必須）. */
  def saveGitAccount(id: Option[UUID], r: GitAccountReq, token: Option[String]): IO[Either[String, GitAccountDto]] =
    tx {
      val aid = id.getOrElse(UUID.randomUUID())
      val q = id match {
        case None =>
          (fr"""insert into git_account (id, name, provider, api_url, username, token, author_name, author_email, auth_kind, app_id, installation_id)
              values ($aid, ${r.name}, ${r.provider}, ${r.apiUrl}, ${r.username}, ${token.getOrElse(
              ""
            )}, ${r.authorName}, ${r.authorEmail},
                      ${r.authKind}, ${r.appId}, ${r.installationId})
              on conflict (name) do nothing returning""" ++ accountCols).query[GitAccountDto].option
        case Some(_) =>
          (fr"""update git_account set name = ${r.name}, provider = ${r.provider}, api_url = ${r.apiUrl}, username = ${r.username},
              author_name = ${r.authorName}, author_email = ${r.authorEmail}, auth_kind = ${r.authKind}, app_id = ${r.appId},
              installation_id = ${r.installationId}, updated_at = now()""" ++
            token.fold(Fragment.empty)(t => fr", token = $t") ++
            fr"where id = $aid and not exists (select 1 from git_account o where o.name = ${r.name} and o.id <> $aid) returning" ++
            accountCols)
            .query[GitAccountDto].option
      }
      q.map(_.toRight(s"名前「${r.name}」のアカウントがすでにあるか、見つかりません"))
    }

  def deleteGitAccount(id: UUID): IO[Boolean] = tx(sql"delete from git_account where id = $id".update.run.map(_ > 0))

  // ---------------------------------------------------------------- クラウドの資格情報

  private val cloudCols = fr"id, name, provider, config, secret <> '', notes, enabled, access, access_groups"
  private type CloudRow = (UUID, String, String, Json, Boolean, String, Boolean, String, List[UUID])
  private def cloudDto(r: CloudRow): CloudCredentialDto =
    CloudCredentialDto(
      r._1,
      r._2,
      r._3,
      r._4.as[Map[String, String]].getOrElse(Map.empty),
      r._5,
      r._6,
      r._7,
      access = r._8,
      groups = r._9
    )

  def cloudCredentials(enabledOnly: Boolean = false): IO[List[CloudCredentialDto]] = tx((fr"select" ++ cloudCols ++
    fr"from cloud_credential" ++ (if enabledOnly then fr"where enabled" else Fragment.empty) ++
    fr"order by name").query[CloudRow].to[List].map(_.map(cloudDto)))

  /** 有効な資格情報と、暗号化した秘密（実行のときだけ使う）. */
  def enabledCloudCredentialsWithSecret: IO[List[(CloudCredentialDto, String)]] = tx((fr"select" ++ cloudCols ++
    fr", secret from cloud_credential where enabled order by name").query[(CloudRow, String)].to[List].map(_.map(
    (r, s) => cloudDto(r) -> s
  )))

  def cloudSecret(id: UUID): IO[Option[String]] =
    tx(sql"select secret from cloud_credential where id = $id".query[String].option)

  /** secret: None は変えない（新しく作るときは必須）. */
  /** groups のときだけ、選んだ種類を保存する. */
  private def groupsOf(r: CloudCredentialReq): List[UUID] =
    if r.access == CloudProvider.AccessGroups then r.groups.distinct else Nil

  def saveCloudCredential(id: Option[UUID],
                          r: CloudCredentialReq,
                          secret: Option[String]): IO[Either[String, CloudCredentialDto]] = tx {
    val cid = id.getOrElse(UUID.randomUUID())
    val q = id match {
      case None =>
        (fr"""insert into cloud_credential (id, name, provider, config, secret, notes, enabled, access, access_groups)
              values ($cid, ${r.name}, ${r.provider}, ${r.config.asJson}, ${secret.getOrElse(
            ""
          )}, ${r.notes}, ${r.enabled}, ${r.access}, ${groupsOf(r)})
              on conflict (name) do nothing returning""" ++ cloudCols)
      case Some(_) =>
        fr"""update cloud_credential set name = ${r.name}, provider = ${r.provider}, config = ${r.config.asJson}, notes = ${r.notes},
             enabled = ${r.enabled}, access = ${r.access}, access_groups = ${groupsOf(r)}, updated_at = now()""" ++
          secret.fold(Fragment.empty)(s => fr", secret = $s") ++
          fr"where id = $cid and not exists (select 1 from cloud_credential o where o.name = ${r.name} and o.id <> $cid) returning" ++
          cloudCols
    }
    q.query[CloudRow].option.map(_.map(cloudDto).toRight(s"名前「${r.name}」の資格情報がすでにあるか、見つかりません"))
  }

  def deleteCloudCredential(id: UUID): IO[Boolean] =
    tx(sql"delete from cloud_credential where id = $id".update.run.map(_ > 0))

  // ---------------------------------------------------------------- 依頼の種類

  private val typeCols = fr"id, key, label, description, icon, fields, instructions, position, enabled"
  private type TypeRow = (UUID, String, String, String, String, Json, String, Int, Boolean)
  private def typeDto(r: TypeRow): RequestTypeDto =
    RequestTypeDto(r._1, r._2, r._3, r._4, r._5, r._6.as[List[FormField]].getOrElse(Nil), r._7, r._8, r._9)

  def requestTypes(enabledOnly: Boolean = false): IO[List[RequestTypeDto]] = tx((fr"select" ++ typeCols ++
    fr"from request_type" ++ (if enabledOnly then fr"where enabled" else Fragment.empty) ++
    fr"order by position, label").query[TypeRow].to[List].map(_.map(typeDto)))

  def requestType(key: String): IO[Option[RequestTypeDto]] =
    tx((fr"select" ++ typeCols ++ fr"from request_type where key = $key").query[TypeRow].option.map(_.map(typeDto)))

  def saveRequestType(id: Option[UUID], r: RequestTypeReq): IO[Either[String, RequestTypeDto]] = tx {
    val tid = id.getOrElse(UUID.randomUUID())
    val q = id match {
      case None =>
        (fr"""insert into request_type (id, key, label, description, icon, fields, instructions, position, enabled)
              values ($tid, ${r.key}, ${r.label}, ${r.description}, ${r.icon}, ${r.fields.asJson}, ${r.instructions}, ${r.position}, ${r.enabled})
              on conflict (key) do nothing returning""" ++ typeCols).query[TypeRow].option
      case Some(_) =>
        (fr"""update request_type set key = ${r.key}, label = ${r.label}, description = ${r.description}, icon = ${r.icon},
              fields = ${r.fields.asJson}, instructions = ${r.instructions}, position = ${r.position}, enabled = ${r.enabled}, updated_at = now()
              where id = $tid and not exists (select 1 from request_type o where o.key = ${r.key} and o.id <> $tid) returning""" ++
          typeCols)
          .query[TypeRow].option
    }
    q.map(_.map(typeDto).toRight(s"キー「${r.key}」の依頼の種類がすでにあるか、見つかりません"))
  }

  def deleteRequestType(id: UUID): IO[Boolean] = tx(sql"delete from request_type where id = $id".update.run.map(_ > 0))

  // ---------------------------------------------------------------- 初めての起動

  /** 依頼の種類・サービスが1つもなければ、既定のものを入れる（複数のノードが同時に起動しても重ならないよう、キーの一意で守る）. */
  // ---------------------------------------------------------------- 利用者の種類

  private val groupCols = fr"id, key, label, description, instructions, idp_groups, is_default, position"

  def userGroups: IO[List[UserGroupDto]] =
    tx((fr"select" ++ groupCols ++ fr"from user_group order by position, label").query[UserGroupDto].to[List])

  /** 既定にしたら、ほかの既定を外す（既定は1つだけ）. */
  def saveUserGroup(id: Option[UUID], r: UserGroupReq): IO[Either[String, UserGroupDto]] = tx {
    val gid = id.getOrElse(UUID.randomUUID())
    val clear = if r.isDefault then
      sql"update user_group set is_default = false where is_default and id <> $gid".update.run.void
    else ().pure[ConnectionIO]
    val q = id match {
      case None =>
        fr"""insert into user_group (id, key, label, description, instructions, idp_groups, is_default, position)
             values ($gid, ${r.key}, ${r.label}, ${r.description}, ${r.instructions}, ${r.idpGroups}, ${r.isDefault}, ${r.position})
             on conflict (key) do nothing returning""" ++ groupCols
      case Some(_) =>
        fr"""update user_group set key = ${r.key}, label = ${r.label}, description = ${r.description}, instructions = ${r.instructions},
             idp_groups = ${r.idpGroups}, is_default = ${r.isDefault}, position = ${r.position}, updated_at = now()
             where id = $gid and not exists (select 1 from user_group o where o.key = ${r.key} and o.id <> $gid) returning""" ++
          groupCols
    }
    clear *> q.query[UserGroupDto].option.map(_.toRight(s"キー「${r.key}」の種類がすでにあるか、見つかりません"))
  }

  def deleteUserGroup(id: UUID): IO[Boolean] = tx(sql"delete from user_group where id = $id".update.run.map(_ > 0))

  def seedDefaults: IO[Unit] = tx(
    sql"select count(*) from request_type".query[Long].unique.flatMap { n =>
      if n > 0 then ().pure[ConnectionIO]
      else
        Defaults.requestTypes.traverse_ { r =>
          sql"""insert into request_type (id, key, label, description, icon, fields, instructions, position)
              values (${UUID.randomUUID()}, ${r.key}, ${r.label}, ${r.description}, ${r.icon}, ${r.fields.asJson}, ${r.instructions}, ${r.position})
              on conflict (key) do nothing""".update.run
        }
    } *>
      sql"select count(*) from user_group".query[Long].unique.flatMap { n =>
        if n > 0 then ().pure[ConnectionIO]
        else
          Defaults.userGroups.traverse_ { g =>
            sql"""insert into user_group (id, key, label, description, instructions, is_default, position)
                values (${UUID.randomUUID()}, ${g.key}, ${g.label}, ${g.description}, ${g.instructions}, ${g.isDefault}, ${g.position})
                on conflict (key) do nothing""".update.run
          }
      } *>
      sql"select count(*) from service".query[Long].unique.flatMap { n =>
        if n > 0 then ().pure[ConnectionIO]
        else
          Defaults.services.zipWithIndex.traverse_ {
            (s, i) =>
              {
                val base = if s.kind == ServiceKind.Github then "https://github.com" else ""
                sql"""insert into service (id, key, name, kind, base_url, description, oauth, manual_token, enabled, position)
                values (${UUID.randomUUID()}, ${s.key}, ${s.name}, ${s.kind}, $base, ${s.description}, ${Defaults.oauthFor(
                    s,
                    base
                  ).asJson},
                        true, false, ${i + 1})
                on conflict (key) do nothing""".update.run
              }
          }
      }
  )
}

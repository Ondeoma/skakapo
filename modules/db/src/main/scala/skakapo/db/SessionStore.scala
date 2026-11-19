package skakapo.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import doobie.postgres.implicits.*
import io.circe.Json
import io.circe.syntax.*
import skakapo.shared.*

import java.time.Instant
import java.util.UUID

/** 会話（transcript は Claude に渡す MessageParam の配列の JSON）. */
final case class AgentSession(
  id: UUID,
  userId: UUID,
  title: String,
  requestType: String,
  form: Map[String, String],
  status: String,
  links: List[LinkDto],
  archived: Boolean,
  createdAt: Instant,
  updatedAt: Instant,
  titleAuto: Boolean) {
  def dto(
    label: String
  ): SessionDto =
    SessionDto(
      id,
      title,
      requestType,
      label,
      status,
      form,
      links,
      createdAt.toString,
      updatedAt.toString,
      archived,
      titleAuto
    )
}

/** 実行（利用者の発言1回ごと. どのノードでも取れる）. */
final case class Run(
  id: UUID,
  sessionId: UUID,
  status: String,
  node: Option[String],
  cancelRequested: Boolean,
  attempts: Int,
  turns: Int,
  inputTokens: Long,
  outputTokens: Long,
  cacheReadTokens: Long,
  error: Option[String],
  createdAt: Instant,
  startedAt: Option[Instant],
  finishedAt: Option[Instant]) {
  def dto: RunDto = RunDto(
    id,
    status,
    node,
    turns,
    inputTokens,
    outputTokens,
    cacheReadTokens,
    error,
    createdAt.toString,
    startedAt.map(_.toString),
    finishedAt.map(_.toString)
  )
}

/** 会話・画面に出す出来事・実行の行列. */
final class SessionStore(
  xa: Transactor[IO]) extends TxStore(xa) {

  private val cols =
    fr"id, user_id, title, request_type, form, status, links, archived, created_at, updated_at, title_auto"
  private type Row = (UUID, UUID, String, String, Json, String, Json, Boolean, Instant, Instant, Boolean)
  private def session(
    r: Row
  ): AgentSession =
    AgentSession(
      r._1,
      r._2,
      r._3,
      r._4,
      r._5.as[Map[String, String]].getOrElse(Map.empty),
      r._6,
      r._7.as[List[LinkDto]].getOrElse(Nil),
      r._8,
      r._9,
      r._10,
      r._11
    )

  /**
   * 会話を作り、最初の発言（出来事と transcript）と実行を入れる. 添付したファイル（本人が上げて、まだ会話に結び付いていないもの）を結び付ける.
   * titleAuto なら題名は仮のもの（エージェントが set_title で決める）.
   */
  def create(
    userId: UUID,
    title: String,
    titleAuto: Boolean,
    requestType: String,
    form: Map[String, String],
    firstMessage: String,
    transcript: Json,
    attachments: List[AttachmentDto] = Nil
  ): IO[(AgentSession, UUID)] = tx {
    val id = UUID.randomUUID()
    for {
      s <-
      (fr"""insert into agent_session (id, user_id, title, title_auto, request_type, form, status, transcript)
                 values ($id, $userId, $title, $titleAuto, $requestType, ${form.asJson}, 'queued', $transcript) returning""" ++
        cols).query[Row].unique.map(session)
      _ <- linkC(id, userId, attachments)
      _ <- eventC(
        id,
        "user",
        Json.obj("text" -> firstMessage.asJson, "form" -> true.asJson, "attachments" -> attachments.asJson)
      )
      run <- enqueueC(id)
    } yield (s, run)
  }

  /** 題名を変える. byAgent なら、まだ仮の題名のときだけ（利用者が付けた題名は上書きしない）. 利用者が変えたら、もう自動にしない. */
  def setTitle(
    id: UUID,
    title: String,
    byAgent: Boolean
  ): IO[Boolean] = tx(
    (if byAgent then sql"update agent_session set title = $title, title_auto = false where id = $id and title_auto"
     else sql"update agent_session set title = $title, title_auto = false where id = $id").update.run.map(_ > 0)
  )

  // ---------------------------------------------------------------- 添付したファイル

  private def linkC(
    sessionId: UUID,
    userId: UUID,
    attachments: List[AttachmentDto]
  ): ConnectionIO[Unit] =
    attachments.traverse_(a =>
      sql"update attachment set session_id = $sessionId where id = ${a.id} and user_id = $userId and session_id is null".update.run
    )

  def saveAttachment(
    userId: UUID,
    name: String,
    contentType: String,
    data: Array[Byte]
  ): IO[AttachmentDto] = tx {
    val id = UUID.randomUUID()
    sql"""insert into attachment (id, user_id, name, content_type, size, data) values ($id, $userId, $name, $contentType, ${data.length}, $data)""".update.run
      .as(AttachmentDto(id, name, contentType, data.length.toLong))
  }

  /** 本人が上げた、まだ会話に結び付いていないファイル（会話を始める・返事を送るときに使う）. */
  def pendingAttachments(
    userId: UUID,
    ids: List[UUID]
  ): IO[List[(AttachmentDto, Array[Byte])]] = tx(
    if ids.isEmpty then List.empty.pure[ConnectionIO]
    else
      sql"""select id, name, content_type, size, data from attachment where user_id = $userId and session_id is null and id = any(${ids.toArray})"""
        .query[(UUID, String, String, Int, Array[Byte])].to[List].map(_.map(r =>
          AttachmentDto(r._1, r._2, r._3, r._4.toLong) -> r._5
        )
          .sortBy(x => ids.indexOf(x._1.id)))
  )

  /** 添付したファイルの名前・種類・中身（本人のもの. 会話に結び付いていれば、その会話の持ち主）. */
  def attachment(
    id: UUID,
    userId: UUID
  ): IO[Option[(String, String, Array[Byte])]] = tx(
    sql"""select a.name, a.content_type, a.data from attachment a left join agent_session s on s.id = a.session_id
          where a.id = $id and (a.user_id = $userId or s.user_id = $userId)""".query[(
      String,
      String,
      Array[Byte]
    )].option
  )

  /** 会話に添付したファイルの合計の大きさ（バイト）. */
  def attachedBytes(
    sessionId: UUID
  ): IO[Long] = tx(
    sql"select coalesce(sum(size), 0)::bigint from attachment where session_id = $sessionId".query[Long].unique
  )

  /** 会話に結び付かないまま1日たったファイルを消す. */
  def purgeAttachments: IO[Unit] =
    tx(sql"delete from attachment where session_id is null and created_at < now() - interval '1 day'".update.run.void)

  def find(
    id: UUID
  ): IO[Option[AgentSession]] =
    tx((fr"select" ++ cols ++ fr"from agent_session where id = $id").query[Row].option.map(_.map(session)))

  def list(
    userId: UUID,
    archived: Boolean
  ): IO[List[AgentSession]] = tx(
    (fr"select" ++ cols ++
      fr"from agent_session where user_id = $userId and archived = $archived order by updated_at desc limit 300").query[
      Row
    ].to[List].map(_.map(session))
  )

  def setArchived(
    id: UUID,
    userId: UUID,
    archived: Boolean
  ): IO[Boolean] = tx(
    sql"update agent_session set archived = $archived where id = $id and user_id = $userId".update.run.map(_ > 0)
  )

  def delete(
    id: UUID,
    userId: UUID
  ): IO[Boolean] = tx(sql"delete from agent_session where id = $id and user_id = $userId".update.run.map(_ > 0))

  def transcript(
    id: UUID
  ): IO[Json] = tx(sql"select transcript from agent_session where id = $id".query[Json].unique)

  /** 実行を、このノードがまだ持っているか（借りが切れてほかのノードに移ったら、もう書かない）. */
  private def owns(
    runId: UUID,
    node: String
  ) =
    fr"exists (select 1 from run where id = $runId and node = $node and status = ${RunStatus.Running})"

  /** 会話の全体を書き換える（修復したときだけ）. 実行を持っていなければ false. */
  def saveTranscript(
    id: UUID,
    transcript: Json,
    runId: UUID,
    node: String
  ): IO[Boolean] = tx(
    (fr"update agent_session set transcript = $transcript, updated_at = now() where id = $id and" ++
      owns(runId, node)).update.run.map(_ > 0)
  )

  /** 会話の末尾に足す（messages は配列）. 実行を持っていなければ false. */
  def appendTranscript(
    id: UUID,
    messages: Json,
    runId: UUID,
    node: String
  ): IO[Boolean] = tx(
    (fr"update agent_session set transcript = transcript || $messages, updated_at = now() where id = $id and" ++
      owns(runId, node)).update.run.map(_ > 0)
  )

  /** 作ったもの（タスク・プルリクエスト）を足す（同じ URL は1つだけ）. */
  def addLink(
    id: UUID,
    link: LinkDto
  ): IO[Unit] = tx(
    sql"select links from agent_session where id = $id for update".query[Json].option.flatMap {
      case None => ().pure[ConnectionIO]
      case Some(j) =>
        val cur = j.as[List[LinkDto]].getOrElse(Nil)
        if cur.exists(_.url == link.url) then ().pure[ConnectionIO]
        else sql"update agent_session set links = ${(cur :+ link).takeRight(50).asJson} where id = $id".update.run.void
    }
  )

  // ---------------------------------------------------------------- 発言（実行を入れる）

  /**
   * 利用者の発言（message は MessageParam の JSON. transcript を読まずに末尾へ足すので、実行の最後の保存と競合しない）.
   * 同じ会話で動いている実行があれば Left（同時に動かさない）.
   */
  def say(
    id: UUID,
    userId: UUID,
    text: String,
    message: Json,
    attachments: List[AttachmentDto] = Nil
  ): IO[Either[String, UUID]] = tx(
    // 同じ会話への発言が重なっても、実行が2つ入らないよう会話の行を押さえてから確かめる
    sql"select 1 from agent_session where id = $id for update".query[Int].option *>
      sql"select 1 from run where session_id = $id and status in (${RunStatus.Queued}, ${RunStatus.Running})".query[
        Int
      ].option.flatMap {
        case Some(_) => "エージェントが作業中です。止めるか、終わるまで待ってください".asLeft[UUID].pure[ConnectionIO]
        case None =>
          sql"""update agent_session set transcript = transcript || jsonb_build_array($message), status = 'queued', updated_at = now()
              where id = $id""".update.run *>
            linkC(id, userId, attachments) *>
            eventC(
              id,
              "user",
              Json.obj("text" -> text.asJson).deepMerge(
                if attachments.isEmpty then Json.obj() else Json.obj("attachments" -> attachments.asJson)
              )
            ) *> enqueueC(id).map(Right(_))
      }
  )

  /** 利用者が同時に待たせている・動かしている実行の数（1人で全ノードの枠を占めないように）. */
  def activeRuns(
    userId: UUID
  ): IO[Int] = tx(
    sql"""select count(*)::int from run r join agent_session s on s.id = r.session_id
          where s.user_id = $userId and r.status in (${RunStatus.Queued}, ${RunStatus.Running})""".query[Int].unique
  )

  private def enqueueC(
    sessionId: UUID
  ): ConnectionIO[UUID] = {
    val rid = UUID.randomUUID()
    sql"insert into run (id, session_id) values ($rid, $sessionId)".update.run.as(rid)
  }

  // ---------------------------------------------------------------- 出来事

  private def eventC(
    sessionId: UUID,
    kind: String,
    data: Json
  ): ConnectionIO[Long] =
    sql"insert into session_event (session_id, kind, data) values ($sessionId, $kind, $data) returning id".query[
      Long
    ].unique

  def event(
    sessionId: UUID,
    kind: String,
    data: Json
  ): IO[Long] = tx(eventC(sessionId, kind, data))

  private def eventDto(
    r: (Long, String, Json, Instant)
  ): EventDto = EventDto(r._1, r._2, r._3, r._4.toString)

  /** 最新の limit 件（古い順. 会話を開いたとき・MCP の get_session）. */
  def latestEvents(
    sessionId: UUID,
    limit: Int = 500
  ): IO[List[EventDto]] = tx(
    sql"""select id, kind, data, created_at from session_event where session_id = $sessionId order by id desc limit $limit"""
      .query[(Long, String, Json, Instant)].to[List].map(_.reverse.map(eventDto))
  )

  /** 長いポーリング: 会話の状態と、after より新しい出来事があるか（1回の問い合わせで）. */
  def pollState(
    id: UUID,
    after: Long
  ): IO[Option[(String, Boolean)]] = tx(
    sql"""select status, exists (select 1 from session_event where session_id = $id and id > $after) from agent_session where id = $id"""
      .query[(String, Boolean)].option
  )

  def events(
    sessionId: UUID,
    after: Long,
    limit: Int = 500
  ): IO[List[EventDto]] = tx(
    sql"""select id, kind, data, created_at from session_event where session_id = $sessionId and id > $after order by id limit $limit"""
      .query[(Long, String, Json, Instant)].to[List].map(_.map(eventDto))
  )

  // ---------------------------------------------------------------- 実行

  private val runCols =
    fr"id, session_id, status, node, cancel_requested, attempts, turns, input_tokens, output_tokens, cache_read_tokens, error, created_at, started_at, finished_at"

  /** 会話の使用量（すべての実行の合計）. */
  def usage(
    sessionId: UUID
  ): IO[UsageDto] = tx(
    sql"""select coalesce(sum(cost_usd), 0)::float8, coalesce(sum(input_tokens), 0)::int8, coalesce(sum(output_tokens), 0)::int8,
                 coalesce(sum(cache_read_tokens), 0)::int8, coalesce(sum(cache_write_tokens), 0)::int8, coalesce(sum(turns), 0)::int4, count(*)::int4
          from run where session_id = $sessionId""".query[UsageDto].unique
  )

  def latestRun(
    sessionId: UUID
  ): IO[Option[Run]] = tx(
    (fr"select" ++ runCols ++ fr"from run where session_id = $sessionId order by created_at desc limit 1").query[
      Run
    ].option
  )

  /**
   * 実行を1つ取る（待っているもの、または動いていたノードの借り（lease）が切れたもの. 3回まで）.
   * 複数のノードが同時に取っても重ならないよう for update skip locked で取る.
   */
  def claim(
    node: String,
    leaseSeconds: Int
  ): IO[Option[Run]] = tx(
    (fr"""update run set status = ${RunStatus.Running}, node = $node, lease_until = now() + make_interval(secs => $leaseSeconds),
            attempts = attempts + 1, started_at = coalesce(started_at, now())
          where id = (select id from run
                      where (status = ${RunStatus.Queued} or (status = ${RunStatus.Running} and lease_until < now())) and attempts < 3 and not cancel_requested
                      order by created_at for update skip locked limit 1)
          returning""" ++ runCols).query[Run].option.flatTap(_.traverse_(r =>
      sql"update agent_session set status = 'running', updated_at = now() where id = ${r.sessionId}".update.run
    ))
  )

  /** 借りを延ばす. 取り消しを頼まれていれば true（ほかのノードに移っていれば None）. */
  def renew(
    id: UUID,
    node: String,
    leaseSeconds: Int
  ): IO[Option[Boolean]] = tx(
    sql"""update run set lease_until = now() + make_interval(secs => $leaseSeconds) where id = $id and node = $node and status = ${RunStatus.Running}
          returning cancel_requested""".query[Boolean].option
  )

  /** 1回の応答のトークンと費用（ドル）を足す. */
  def addUsage(
    id: UUID,
    userId: UUID,
    model: String,
    input: Long,
    output: Long,
    cacheRead: Long,
    cacheWrite: Long,
    costUsd: Double
  ): IO[Unit] = tx(
    sql"""update run set turns = turns + 1, model = $model, input_tokens = input_tokens + $input, output_tokens = output_tokens + $output,
          cache_read_tokens = cache_read_tokens + $cacheRead, cache_write_tokens = cache_write_tokens + $cacheWrite,
          cost_usd = cost_usd + $costUsd where id = $id""".update.run *>
      // クレジットの台帳（会話を消しても残る）
      sql"insert into credit_usage (user_id, run_id, model, cost_usd) values ($userId, $id, $model, $costUsd)".update.run.void
  )

  /**
   * 実行を終える（会話の状態と、終わりの出来事も書く. done は返事待ちにする）.
   * このノードが持っていなければ（取り消された・ほかのノードに移った）何もせず false.
   */
  def finish(
    id: UUID,
    sessionId: UUID,
    node: String,
    status: String,
    error: Option[String]
  ): IO[Boolean] = tx(
    sql"update run set status = $status, error = $error, finished_at = now(), lease_until = null where id = $id and node = $node and status = ${RunStatus.Running}"
      .update.run.flatMap(n => if n == 0 then false.pure[ConnectionIO] else endedC(sessionId, status).as(true))
  )

  /** 実行が終わったことを会話に書く（状態と、画面の「終わった」の合図）. */
  private def endedC(
    sessionId: UUID,
    status: String
  ): ConnectionIO[Unit] =
    sql"""update agent_session set status = ${
        if status == RunStatus.Failed then SessionStatus.Failed else SessionStatus.Idle
      }, updated_at = now()
          where id = $sessionId""".update.run *>
      eventC(sessionId, "status", Json.obj("text" -> RunStatus.label(status).asJson, "done" -> true.asJson)).void

  /** 借りの切れた、取り消しを頼まれた実行を取り消しにしてから、待っている実行があるかを返す. */
  def sweepAndHasQueued: IO[Boolean] = tx(
    sql"""update run set status = ${RunStatus.Cancelled}, finished_at = now() where status = ${RunStatus.Running} and lease_until < now() and cancel_requested
          returning session_id""".query[UUID].to[List].flatMap(_.traverse_(endedC(_, RunStatus.Cancelled))) *>
      sql"select exists (select 1 from run where status = ${RunStatus.Queued} or (status = ${RunStatus.Running} and lease_until < now()))".query[
        Boolean
      ].unique
  )

  /** 取り消しを頼む（待っている実行はすぐ取り消す）. */
  def cancel(
    sessionId: UUID
  ): IO[Boolean] = tx(
    for {
      q <-
        sql"""update run set status = 'cancelled', finished_at = now() where session_id = $sessionId and status = 'queued' returning session_id"""
          .query[UUID].to[List]
      _ <- if q.nonEmpty then sql"update agent_session set status = 'idle' where id = $sessionId".update.run.void
      else ().pure[ConnectionIO]
      r <- sql"update run set cancel_requested = true where session_id = $sessionId and status = 'running'".update.run
    } yield q.nonEmpty || r > 0
  )

  /** 3回試しても終わらなかった実行（ノードが続けて止まった）を失敗にする. */
  def failAbandoned: IO[List[UUID]] = tx(
    sql"""update run set status = 'failed', error = 'ノードが止まったため、3回試しても終わりませんでした', finished_at = now()
          where status = 'running' and lease_until < now() and attempts >= 3 returning session_id""".query[
      UUID
    ].to[List].flatTap(
      _.traverse_(endedC(_, RunStatus.Failed))
    )
  )
}

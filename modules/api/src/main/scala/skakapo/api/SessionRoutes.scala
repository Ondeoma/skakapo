package skakapo.api

import cats.effect.*
import cats.syntax.all.*
import com.anthropic.models.messages.{ContentBlockParam, MessageParam, TextBlockParam}
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skakapo.agent.{Jsons, Workspace}
import skakapo.core.ErrorCode
import skakapo.core.Prompt
import skakapo.db.*
import skakapo.shared.*

import java.nio.file.Path
import java.util.UUID
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** 会話: 依頼の種類（フォーム）・会話を始める・返事・出来事の取得（長いポーリング）・止める. */
final class SessionRoutes(
  store: SessionStore,
  config: ConfigStore,
  val sessions: SessionAuth,
  workBase: Path,
  credits: skakapo.agent.Credits)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "request-types" => getRequestTypes(req)
    case req @ GET -> Root / "api" / "repositories" => getRepositories(req)
    case req @ GET -> Root / "api" / "sessions" => getSessions(req)
    case req @ POST -> Root / "api" / "sessions" => postSessions(req)
    case req @ GET -> Root / "api" / "sessions" / UuidVar(id) => getSession(req, id)
    case req @ GET -> Root / "api" / "sessions" / UuidVar(id) / "events" => getSessionEvents(req, id)
    case req @ POST -> Root / "api" / "sessions" / UuidVar(id) / "messages" => postSessionMessages(req, id)
    case req @ GET -> Root / "api" / "me" / "credit" => getMeCredit(req)
    case req @ POST -> Root / "api" / "sessions" / UuidVar(id) / "cancel" => postSessionCancel(req, id)
    case req @ PATCH -> Root / "api" / "sessions" / UuidVar(id) => patchSession(req, id)
    case req @ POST -> Root / "api" / "attachments" => postAttachments(req)
    case req @ GET -> Root / "api" / "attachments" / UuidVar(id) => getAttachment(req, id)
    case req @ GET -> Root / "api" / "me" / "context" => getMeContext(req)
    case req @ PUT -> Root / "api" / "me" / "context" => putMeContext(req)
    case req @ DELETE -> Root / "api" / "sessions" / UuidVar(id) => deleteSession(req, id)
  }

  private def getRequestTypes(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(_ => config.requestTypes(enabledOnly = true).flatMap(Ok(_)))

  private def getRepositories(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(_ => config.repositories(enabledOnly = true).flatMap(rs => Ok(rs.map(_.copy(notes = "")))))

  private def getSessions(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u =>
      (store.list(u.id, param(req, "archived").contains("true")), labels).flatMapN(
        (
          ss,
          ls
        ) =>
          Ok(ss.map(s => s.dto(ls.getOrElse(s.requestType, s.requestType))))
      )
    )

  private def postSessions(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u =>
      withCredit(u)(body[StartReq](req) { r =>
        config.requestType(r.requestType).flatMap {
          case None => err(ErrorCode.SessionRequestTypeNotFound)
          case Some(t) if !t.enabled => err(ErrorCode.RequestTypeDisabled)
          case Some(t) =>
            valid(FormCheck.values(t.fields, r.values)) { values =>
              checkRefs(t, values).flatMap(valid(_) { _ =>
                withAttachments(u, r.attachments, 0L) { atts =>
                  val first = Prompt.formMessage(t, values)
                  val (title, auto) = Prompt.title(t, values)
                  store.create(
                    u.id,
                    title,
                    auto,
                    t.key,
                    values,
                    first,
                    Jsons.encodeTranscript(List(userMessage(atts, first))),
                    atts.map(_._1)
                  ).flatMap(
                    (
                      s,
                      _
                    ) =>
                      sbirds.observe.Metrics.count("skakapo.sessions.created", "request_type" -> t.key) *>
                        Created(s.dto(t.label))
                  )
                }
              })
            }
        }
      })
    )

  private def getSession(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u => owned(u, id)(s => snapshot(s, 0L).flatMap(Ok(_))))

  private def getSessionEvents(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u =>
      owned(u, id) { s =>
        val after = param(req, "after").flatMap(_.toLongOption).getOrElse(0L)
        val wait = param(req, "wait").flatMap(_.toIntOption).getOrElse(0).max(0).min(25)
        // 待つ間は、新しい出来事と状態だけを見る（DB への問い合わせを減らす）. 変わったら全体を返す
        def poll(
          left: Int
        ): IO[Unit] =
          store.pollState(s.id, after).flatMap {
            case Some((st, fresh)) if !fresh && left > 0 && st == s.status => IO.sleep(1.second) *> poll(left - 1)
            case _ => IO.unit
          }
        poll(wait) *> snapshot(s, after).flatMap(Ok(_))
      }
    )

  private def postSessionMessages(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u =>
      withCredit(u)(owned(u, id)(s =>
        body[MessageReq](req) { r =>
          val text = r.text.trim
          if text.isEmpty && r.attachments.isEmpty then err(ErrorCode.ReplyEmpty)
          else if text.length > 20000 then err(ErrorCode.ReplyTooLong)
          else
            store.attachedBytes(s.id).flatMap(used =>
              withAttachments(u, r.attachments, used) { atts =>
                val message = Jsons.encodeTranscript(List(userMessage(
                  atts,
                  if text.isEmpty then "（添付したファイルを見てください）" else text
                )))
                  .asArray.flatMap(_.headOption).getOrElse(Json.Null)
                store.say(s.id, u.id, text, message, atts.map(_._1)).flatMap {
                  case Right(_) => Accepted(Json.obj())
                  case Left(e) => err(ErrorCode.SessionBusy, e)
                }
              }
            )
        }
      ))
    )

  private def getMeCredit(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u => credits.of(u.id).flatMap(Ok(_)))

  private def postSessionCancel(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u =>
      owned(u, id)(s => store.cancel(s.id).ifM(Accepted(Json.obj()), err(ErrorCode.NothingRunning)))
    )

  private def patchSession(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u =>
      owned(u, id)(s =>
        body[Json](req) { j =>
          val c = j.hcursor
          (c.get[Boolean]("archived").toOption, c.get[String]("title").toOption.map(_.trim)) match {
            case (_, Some(t)) if t.isEmpty || t.length > 100 => err(ErrorCode.SessionTitleInvalid)
            case (a, Some(t)) => store.setTitle(s.id, t, byAgent = false) *>
                a.traverse_(store.setArchived(s.id, u.id, _)) *> NoContent()
            case (Some(a), None) => store.setArchived(s.id, u.id, a) *> NoContent()
            case _ => err(ErrorCode.SessionPatchEmpty)
          }
        }
      )
    )

  private def postAttachments(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req) { u =>
      // 種類が分からない・おかしいときは application/octet-stream として預かる
      val ct = req.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}".toLowerCase)
        .filter(_.matches("[a-z0-9!#$&^_.+-]+/[a-z0-9!#$&^_.+-]+")).getOrElse("application/octet-stream")
      val name = param(req, "name").map(_.trim.take(200)).filter(_.nonEmpty).getOrElse("file")
      req.body.take(AttachmentRule.MaxBytes + 1L).compile.to(Array).flatMap { data =>
        AttachmentRule.check(ct, data.length.toLong) match {
          case Left(e) => err(ErrorCode.AttachmentTooLarge, e)
          case Right(_) =>
            if AttachmentRule.ImageTypes.contains(ct) && skakapo.shared.AvatarImage.detect(data).forall(_ != ct)
            then
              err(ErrorCode.ImageTypeMismatch)
            else store.saveAttachment(u.id, name, ct, data).flatMap(Created(_))
        }
      }
    }

  private def getAttachment(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u =>
      store.attachment(id, u.id).flatMap {
        case None => err(ErrorCode.FileNotFound)
        case Some((name, ct, data)) =>
          val image = AttachmentRule.ImageTypes.contains(ct)
          val disposition =
            if image then "inline"
            else s"attachment; filename*=UTF-8''${java.net.URLEncoder.encode(name, "UTF-8").replace("+", "%20")}"
          IO.pure(Response[IO](Status.Ok).withBodyStream(fs2.Stream.emits(data)).putHeaders(
            headers.`Content-Type`(
              MediaType.parse(ct).toOption.filter(_ => image).getOrElse(MediaType.application.`octet-stream`)
            ),
            "Content-Disposition" -> disposition,
            "Cache-Control" -> "private, max-age=31536000, immutable",
            "X-Content-Type-Options" -> "nosniff",
            "Content-Security-Policy" -> "default-src 'none'"
          ))
      }
    )

  private def getMeContext(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u => config.userContext(u.id).flatMap(t => Ok(UserContextDto(t))))

  private def putMeContext(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u =>
      body[UserContextDto](req)(r =>
        valid(Check.text(r.text, "前提", 20000))(t => config.saveUserContext(u.id, t) *> Ok(UserContextDto(t)))
      )
    )

  private def deleteSession(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u =>
      owned(u, id) { s =>
        store.cancel(s.id) *> store.delete(s.id, u.id) *> Workspace.forSession(workBase, s.id).remove.attempt *>
          NoContent()
      }
    )

  /** 1人が同時に待たせる・動かす実行の数（1人で全ノードの枠を占めないように）. */
  private val MaxActiveRuns = 3

  /** 今月のクレジットが残っていなければ 402、同時の実行が多すぎれば 429（依頼を始める・返事をする前に）. */
  private def withCredit(
    u: User
  )(
    f: => IO[Response[IO]]
  ): IO[Response[IO]] =
    credits.of(u.id).map(_.blocked).flatMap {
      case Some(msg) => err(ErrorCode.CreditExhausted, msg)
      case None => store.activeRuns(u.id).flatMap(n =>
          if n >= MaxActiveRuns then
            err(ErrorCode.TooManyActiveRuns, s"同時に進められる依頼は${MaxActiveRuns}つまでです。終わるのを待つか、止めてください")
          else f
        )
    }

  private def labels: IO[Map[String, String]] = config.requestTypes().map(_.map(t => t.key -> t.label).toMap)

  private def owned(
    u: User,
    id: UUID
  )(
    f: AgentSession => IO[Response[IO]]
  ): IO[Response[IO]] =
    store.find(id).flatMap {
      case Some(s) if s.userId == u.id => f(s)
      case _ => err(ErrorCode.SessionNotFound)
    }

  /** フォームの service・repository の項目は、有効なサービス・登録したリポジトリから選んだものだけ. */
  private def checkRefs(
    rt: RequestTypeDto,
    values: Map[String, String]
  ): IO[Either[String, Unit]] =
    config.enabledRefs.map {
      (
        ss,
        rs
      ) =>
        rt.fields.collectFirst {
          case f if f.kind == FieldKind.Service && values.get(f.name).exists(v => !ss.contains(v)) =>
            s"「${f.label}」は一覧から選んでください"
          case f if f.kind == FieldKind.Repository && values.get(f.name).exists(v => !rs.contains(v)) =>
            s"「${f.label}」は一覧から選んでください"
        }.toLeft(())
    }

  /** 会話の様子. after が 0 なら最新の500件（長い会話でも新しいものを出す）. */
  private def snapshot(
    s: AgentSession,
    after: Long
  ): IO[EventsDto] =
    (
      config.requestType(s.requestType),
      if after <= 0 then store.latestEvents(s.id) else store.events(s.id, after),
      store.latestRun(s.id),
      if after <= 0 then IO.pure(Some(s)) else store.find(s.id)
    )
      .flatMapN {
        (
          rt,
          es,
          run,
          cur
        ) =>
          {
            val x = cur.getOrElse(s)
            // 使用量は、開いたときと新しい出来事があったときだけ集計する（長いポーリングのたびに数えない）
            (if after <= 0 || es.nonEmpty then store.usage(s.id).map(Some(_)) else IO.pure(None))
              .map(usage => EventsDto(x.dto(rt.map(_.label).getOrElse(x.requestType)), es, run.map(_.dto), usage))
          }
      }

  /** 添付の ID を確かめて中身を読む（本人が上げて、まだ会話に結び付いていないもの. used はその会話にすでに添付した合計）. */
  private def withAttachments(
    u: User,
    ids: List[UUID],
    used: Long
  )(
    f: List[(AttachmentDto, Array[Byte])] => IO[Response[IO]]
  ): IO[Response[IO]] = {
    val distinct = ids.distinct
    if distinct.size > AttachmentRule.MaxCount then
      err(ErrorCode.TooManyAttachments, s"添付は1回に ${AttachmentRule.MaxCount} 個までです")
    else
      store.pendingAttachments(u.id, distinct).flatMap { atts =>
        if atts.size != distinct.size then err(ErrorCode.AttachmentNotFound)
        else if used + atts.map(_._1.size).sum > AttachmentRule.MaxTotalBytes then
          err(ErrorCode.AttachmentsTooLarge, s"1つの依頼に添付できるのは合計 ${AttachmentRule.mb(AttachmentRule.MaxTotalBytes)} までです")
        else f(atts)
      }
  }

  /** 利用者の発言（添付は文より前に置く. Claude は添付のあとの指示をよく読む）. */
  private def userMessage(
    atts: List[(AttachmentDto, Array[Byte])],
    text: String
  ): MessageParam =
    if atts.isEmpty then MessageParam.ofUser(text)
    else
      MessageParam.ofUserOfContentBlockParams((AttachmentBlocks(atts) :+
        ContentBlockParam.ofText(TextBlockParam.builder().text(text).build())).asJava)

}

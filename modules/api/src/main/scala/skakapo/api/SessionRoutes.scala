package skakapo.api

import cats.effect.*
import cats.syntax.all.*
import com.anthropic.models.messages.{Base64ImageSource, ContentBlockParam, ImageBlockParam, MessageParam, TextBlockParam}
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skakapo.agent.{Jsons, Workspace}
import skakapo.core.Prompt
import skakapo.db.*
import skakapo.shared.*

import java.nio.file.Path
import java.util.UUID
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** 会話: 依頼の種類（フォーム）・会話を始める・返事・出来事の取得（長いポーリング）・止める. */
final class SessionRoutes(store: SessionStore, config: ConfigStore, val sessions: SessionAuth, workBase: Path, credits: skakapo.agent.Credits)
    extends RouteSupport:

  /** 1人が同時に待たせる・動かす実行の数（1人で全ノードの枠を占めないように）. */
  private val MaxActiveRuns = 3

  /** 今月のクレジットが残っていなければ 402、同時の実行が多すぎれば 429（依頼を始める・返事をする前に）. */
  private def withCredit(u: User)(f: => IO[Response[IO]]): IO[Response[IO]] =
    credits.of(u.id).map(_.blocked).flatMap {
      case Some(msg) => err(Status.PaymentRequired, msg)
      case None => store.activeRuns(u.id).flatMap(n =>
          if n >= MaxActiveRuns then err(Status.TooManyRequests, s"同時に進められる依頼は${MaxActiveRuns}つまでです。終わるのを待つか、止めてください") else f)
    }

  private def labels: IO[Map[String, String]] = config.requestTypes().map(_.map(t => t.key -> t.label).toMap)

  private def owned(u: User, id: UUID)(f: AgentSession => IO[Response[IO]]): IO[Response[IO]] =
    store.find(id).flatMap {
      case Some(s) if s.userId == u.id => f(s)
      case _                           => err(Status.NotFound, "会話が見つかりません")
    }

  /** フォームの service・repository の項目は、有効なサービス・登録したリポジトリから選んだものだけ. */
  private def checkRefs(rt: RequestTypeDto, values: Map[String, String]): IO[Either[String, Unit]] =
    config.enabledRefs.map { (ss, rs) =>
      rt.fields.collectFirst {
        case f if f.kind == FieldKind.Service && values.get(f.name).exists(v => !ss.contains(v)) => s"「${f.label}」は一覧から選んでください"
        case f if f.kind == FieldKind.Repository && values.get(f.name).exists(v => !rs.contains(v)) => s"「${f.label}」は一覧から選んでください"
      }.toLeft(())
    }

  /** 会話の様子. after が 0 なら最新の500件（長い会話でも新しいものを出す）. */
  private def snapshot(s: AgentSession, after: Long): IO[EventsDto] =
    (config.requestType(s.requestType), if after <= 0 then store.latestEvents(s.id) else store.events(s.id, after), store.latestRun(s.id),
      if after <= 0 then IO.pure(Some(s)) else store.find(s.id))
      .flatMapN { (rt, es, run, cur) =>
        val x = cur.getOrElse(s)
        // 使用量は、開いたときと新しい出来事があったときだけ集計する（長いポーリングのたびに数えない）
        (if after <= 0 || es.nonEmpty then store.usage(s.id).map(Some(_)) else IO.pure(None))
          .map(usage => EventsDto(x.dto(rt.map(_.label).getOrElse(x.requestType)), es, run.map(_.dto), usage))
      }

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "request-types" => authed(req)(_ => config.requestTypes(enabledOnly = true).flatMap(Ok(_)))

    // フォームの「リポジトリ」の選択肢（作業の決まりは管理者だけに見せる）
    case req @ GET -> Root / "api" / "repositories" =>
      authed(req)(_ => config.repositories(enabledOnly = true).flatMap(rs => Ok(rs.map(_.copy(notes = "")))))

    case req @ GET -> Root / "api" / "sessions" =>
      authed(req)(u => (store.list(u.id, param(req, "archived").contains("true")), labels).flatMapN((ss, ls) =>
        Ok(ss.map(s => s.dto(ls.getOrElse(s.requestType, s.requestType))))))

    case req @ POST -> Root / "api" / "sessions" =>
      authed(req)(u => withCredit(u)(body[StartReq](req) { r =>
        config.requestType(r.requestType).flatMap {
          case None                => err(Status.BadRequest, "依頼の種類が見つかりません")
          case Some(t) if !t.enabled => err(Status.BadRequest, "この依頼の種類は使えません")
          case Some(t) =>
            valid(FormCheck.values(t.fields, r.values)) { values =>
              checkRefs(t, values).flatMap(valid(_) { _ =>
                if r.attachments.distinct.size > AttachmentRule.MaxCount then err(Status.BadRequest, s"画像は ${AttachmentRule.MaxCount} 枚までです")
                else store.pendingAttachments(u.id, r.attachments.distinct).flatMap { atts =>
                  if atts.size != r.attachments.distinct.size then err(Status.BadRequest, "添付した画像が見つかりません。もう一度添付してください")
                  else
                    val first = Prompt.formMessage(t, values)
                    val (title, auto) = Prompt.title(t, values)
                    // 画像は、フォームの文より前に置く（Claude は画像のあとの指示をよく読む）
                    val blocks = atts.map((a, data) => ContentBlockParam.ofImage(ImageBlockParam.builder().source(Base64ImageSource.builder()
                      .mediaType(Base64ImageSource.MediaType.of(a.contentType)).data(java.util.Base64.getEncoder.encodeToString(data)).build()).build())) :+
                      ContentBlockParam.ofText(TextBlockParam.builder().text(first).build())
                    store.create(u.id, title, auto, t.key, values, first, Jsons.encodeTranscript(List(MessageParam.ofUserOfContentBlockParams(blocks.asJava))),
                      atts.map(_._1)).flatMap((s, _) => skakapo.observe.Metrics.count("skakapo.sessions.created", "request_type" -> t.key) *> Created(s.dto(t.label)))
                }
              })
            }
        }
      }))

    case req @ GET -> Root / "api" / "sessions" / UuidVar(id) =>
      authed(req)(u => owned(u, id)(s => snapshot(s, 0L).flatMap(Ok(_))))

    // 新しい出来事. wait 秒（最大25）まで、新しいものが出るか状態が変わるのを待つ（どのノードに来てもよいよう、DB を1秒ごとに見る）
    case req @ GET -> Root / "api" / "sessions" / UuidVar(id) / "events" =>
      authed(req)(u => owned(u, id) { s =>
        val after = param(req, "after").flatMap(_.toLongOption).getOrElse(0L)
        val wait  = param(req, "wait").flatMap(_.toIntOption).getOrElse(0).max(0).min(25)
        // 待つ間は、新しい出来事と状態だけを見る（DB への問い合わせを減らす）. 変わったら全体を返す
        def poll(left: Int): IO[Unit] =
          store.pollState(s.id, after).flatMap {
            case Some((st, fresh)) if !fresh && left > 0 && st == s.status => IO.sleep(1.second) *> poll(left - 1)
            case _ => IO.unit
          }
        poll(wait) *> snapshot(s, after).flatMap(Ok(_))
      })

    case req @ POST -> Root / "api" / "sessions" / UuidVar(id) / "messages" =>
      authed(req)(u => withCredit(u)(owned(u, id)(s => body[MessageReq](req) { r =>
        val text = r.text.trim
        if text.isEmpty then err(Status.BadRequest, "返事を入れてください")
        else if text.length > 20000 then err(Status.BadRequest, "返事は20000文字までにしてください")
        else
          val message = Jsons.encodeTranscript(List(MessageParam.ofUser(text))).asArray.flatMap(_.headOption).getOrElse(Json.Null)
          store.say(s.id, text, message).flatMap {
            case Right(_) => Accepted(Json.obj())
            case Left(e)  => err(Status.Conflict, e)
          }
      })))

    // 今月のクレジット（自分の使った額・上限・残りと、全体の予算・残り）
    case req @ GET -> Root / "api" / "me" / "credit" => authed(req)(u => credits.of(u.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "sessions" / UuidVar(id) / "cancel" =>
      authed(req)(u => owned(u, id)(s => store.cancel(s.id).ifM(Accepted(Json.obj()), err(Status.Conflict, "動いている作業はありません"))))

    // しまう・戻す（archived）と、題名を変える（title. 変えたらエージェントはもう題名を決めない）
    case req @ PATCH -> Root / "api" / "sessions" / UuidVar(id) =>
      authed(req)(u => owned(u, id)(s => body[Json](req) { j =>
        val c = j.hcursor
        (c.get[Boolean]("archived").toOption, c.get[String]("title").toOption.map(_.trim)) match
          case (_, Some(t)) if t.isEmpty || t.length > 100 => err(Status.BadRequest, "題名を100文字以内で入れてください")
          case (a, Some(t)) => store.setTitle(s.id, t, byAgent = false) *> a.traverse_(store.setArchived(s.id, u.id, _)) *> NoContent()
          case (Some(a), None) => store.setArchived(s.id, u.id, a) *> NoContent()
          case _ => err(Status.BadRequest, "archived か title を指定してください")
      }))

    // ------------------------------------------------------------ 画像の添付（会話を始める前に上げて、ID を StartReq に入れる）
    case req @ POST -> Root / "api" / "attachments" =>
      authed(req) { u =>
        val ct = req.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}").getOrElse("")
        val name = param(req, "name").map(_.take(200)).getOrElse("image")
        if !AttachmentRule.Types.contains(ct) then err(Status.UnsupportedMediaType, "PNG・JPEG・GIF・WebP の画像を選んでください")
        else req.body.take(AttachmentRule.MaxBytes + 1L).compile.to(Array).flatMap { data =>
          if data.length > AttachmentRule.MaxBytes then err(Status.PayloadTooLarge, "画像は5MBまでにしてください")
          else if skakapo.shared.AvatarImage.detect(data).forall(_ != ct) then err(Status.UnsupportedMediaType, "画像の中身が種類と合いません")
          else store.saveAttachment(u.id, name, ct, data).flatMap(Created(_))
        }
      }

    case req @ GET -> Root / "api" / "attachments" / UuidVar(id) =>
      authed(req)(u => store.attachment(id, u.id).flatMap {
        case None => err(Status.NotFound, "画像が見つかりません")
        case Some((ct, data)) =>
          IO.pure(Response[IO](Status.Ok).withBodyStream(fs2.Stream.emits(data)).putHeaders(headers.`Content-Type`(MediaType.unsafeParse(ct)),
            "Cache-Control" -> "private, max-age=31536000, immutable", "X-Content-Type-Options" -> "nosniff", "Content-Security-Policy" -> "default-src 'none'"))
      })

    // ------------------------------------------------------------ 自分で登録する、エージェントへの前提
    case req @ GET -> Root / "api" / "me" / "context" => authed(req)(u => config.userContext(u.id).flatMap(t => Ok(UserContextDto(t))))

    case req @ PUT -> Root / "api" / "me" / "context" =>
      authed(req)(u => body[UserContextDto](req)(r => valid(Check.text(r.text, "前提", 20000))(t => config.saveUserContext(u.id, t) *> Ok(UserContextDto(t)))))

    case req @ DELETE -> Root / "api" / "sessions" / UuidVar(id) =>
      authed(req)(u => owned(u, id) { s =>
        store.cancel(s.id) *> store.delete(s.id, u.id) *> Workspace.forSession(workBase, s.id).remove.attempt *> NoContent()
      })
  }

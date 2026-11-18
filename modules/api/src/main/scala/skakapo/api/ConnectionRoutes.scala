package skakapo.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Location
import skakapo.agent.Connections
import skakapo.db.*
import skakapo.shared.*

import java.util.UUID

/** 利用者の連携（各サービスに OAuth でトークンを発行してもらう・API トークンを貼り付ける・やめる）. */
final class ConnectionRoutes(conns: Connections, store: ConnectionStore, config: ConfigStore, val sessions: SessionAuth) extends RouteSupport:

  private def service(id: UUID)(f: ServiceDto => IO[Response[IO]]): IO[Response[IO]] =
    config.service(id).flatMap {
      case Some(s) if s.enabled => f(s)
      case _                    => err(Status.NotFound, "サービスが見つかりません")
    }

  private def page(title: String, msg: String): IO[Response[IO]] = HtmlPage(title, msg, Status.BadRequest, "/connections", "連携の画面へ戻る")

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "connections" =>
      authed(req)(u => (config.services(enabledOnly = true), store.list(u.id)).flatMapN { (ss, cs) =>
        Ok(ss.map { s =>
          val c = cs.find(_.serviceId == s.id)
          ConnectionDto(s.copy(oauth = s.oauth.copy(clientId = "")), c.isDefined, c.map(_.via), c.map(_.scope).getOrElse(""),
            c.map(_.connectedAt.toString), s.oauth.usable && s.hasSecret)
        })
      })

    // OAuth を始める. 画面はこの URL へ移動する
    case req @ POST -> Root / "api" / "connections" / UuidVar(id) / "oauth" =>
      interactive(req)(u => service(id)(s =>
        conns.start(u.id, s, SafePath.of(param(req, "returnTo"), "/connections")).flatMap {
          case Left(e)  => err(Status.BadRequest, e)
          case Right(r) => Ok(Json.obj("url" -> r.renderString.asJson))
        }))

    // 各サービスから戻ってきたところ（ブラウザのリダイレクト. ログインしている Cookie で、始めた本人か確かめる）
    case req @ GET -> Root / "api" / "connections" / "callback" =>
      sessions.fromRequest(req).flatMap {
        case None => page("連携できませんでした", "ログインが切れました。ログインしてから、もう一度連携してください。")
        case Some(p) =>
          (param(req, "state"), param(req, "code"), param(req, "error")) match
            case (_, _, Some(e)) => page("連携できませんでした", s"連携先が許可しませんでした（${e.take(60)}）。")
            case (Some(st), Some(code), None) =>
              conns.finish(st, code, p.user.id).flatMap {
                case Left(e) => page("連携できませんでした", e)
                case Right((s, back)) =>
                  skakapo.observe.Metrics.count("skakapo.connections.created", "service" -> s.key, "via" -> "oauth") *>
                    IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(
                      back + (if back.contains("?") then "&" else "?") + s"connected=${s.key}"))))
              }
            case _ => page("連携できませんでした", "連携先からの応答が足りません。もう一度連携してください。")
      }

    case req @ PUT -> Root / "api" / "connections" / UuidVar(id) / "token" =>
      interactive(req)(u => service(id)(s => body[ManualTokenReq](req)(r =>
        conns.saveManual(u.id, s, r.token).flatMap {
          case Left(e)  => err(Status.BadRequest, e)
          case Right(_) => skakapo.observe.Metrics.count("skakapo.connections.created", "service" -> s.key, "via" -> "manual") *> NoContent()
        })))

    case req @ DELETE -> Root / "api" / "connections" / UuidVar(id) =>
      interactive(req)(u => conns.disconnect(u.id, id).ifM(NoContent(), err(Status.NotFound, "連携していません")))

    // ツールの一覧を読み直す（サービスが新しいツールを足したとき）
    case req @ POST -> Root / "api" / "connections" / UuidVar(id) / "refresh" =>
      authed(req)(u => service(id)(s => conns.token(u.id, s.id).flatMap {
        case None => err(Status.BadRequest, "連携していません")
        case Some(t) =>
          conns.forget(u.id, s.id) *> conns.learnTools(s, t).attempt.flatMap {
            case Right(ts) => Ok(Json.obj("tools" -> ts.size.asJson))
            case Left(e)   => err(Status.BadGateway, s"ツールを読めませんでした: ${skakapo.core.Text.errMsg(e, 300)}")
          }
      }))
  }

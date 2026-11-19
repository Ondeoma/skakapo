package skakapo.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Location
import skakapo.agent.Connections
import skakapo.core.ErrorCode
import skakapo.db.*
import skakapo.shared.*

import java.util.UUID

/** 利用者の連携（各サービスに OAuth でトークンを発行してもらう・API トークンを貼り付ける・やめる）. */
final class ConnectionRoutes(conns: Connections, store: ConnectionStore, config: ConfigStore, val sessions: SessionAuth)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "connections" => getConnections(req)
    case req @ POST -> Root / "api" / "connections" / UuidVar(id) / "oauth" => postConnectionOauth(req, id)
    case req @ GET -> Root / "api" / "connections" / "callback" => getConnectionsCallback(req)
    case req @ PUT -> Root / "api" / "connections" / UuidVar(id) / "token" => putConnectionToken(req, id)
    case req @ DELETE -> Root / "api" / "connections" / UuidVar(id) => deleteConnection(req, id)
    case req @ POST -> Root / "api" / "connections" / UuidVar(id) / "refresh" => postConnectionRefresh(req, id)
  }

  private def getConnections(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u =>
      (config.services(enabledOnly = true), store.list(u.id), sessions.appNames(req)).flatMapN {
        (ss, cs, names) =>
          Ok(ss.map { s =>
            val c = cs.find(_.serviceId == s.id)
            // sbirds のサービスの名前は sruru に登録した表示名（なければ管理画面で付けた名前）
            val name =
              if (s.kind == ServiceKind.Sbirds) names.get(s.key).filter(_.trim.nonEmpty).getOrElse(s.name) else s.name
            ConnectionDto(
              s.copy(name = name, oauth = s.oauth.copy(clientId = "")),
              c.isDefined,
              c.map(_.via),
              c.map(_.scope).getOrElse(""),
              c.map(_.connectedAt.toString),
              s.oauth.usable && s.hasSecret
            )
          })
      }
    )

  private def postConnectionOauth(req: Request[IO], id: UUID): IO[Response[IO]] =
    interactive(req)(u =>
      service(id)(s =>
        conns.start(u.id, s, SafePath.of(param(req, "returnTo"), "/connections")).flatMap {
          case Left(e) => err(ErrorCode.ConnectionFailed, e)
          case Right(r) => Ok(Json.obj("url" -> r.renderString.asJson))
        }
      )
    )

  private def getConnectionsCallback(req: Request[IO]): IO[Response[IO]] =
    sessions.fromRequest(req).flatMap {
      case None => page(ErrorCode.ConnectionLoginExpired)
      case Some(p) =>
        (param(req, "state"), param(req, "code"), param(req, "error")) match {
          case (_, _, Some(e)) => page(ErrorCode.ConnectionDeniedByService, s"連携先が許可しませんでした（${e.take(60)}）。")
          case (Some(st), Some(code), None) =>
            conns.finish(st, code, p.user.id).flatMap {
              case Left(e) => page(ErrorCode.ConnectionFailed, e)
              case Right((s, back)) =>
                sbirds.observe.Metrics.count("skakapo.connections.created", "service" -> s.key, "via" -> "oauth") *>
                  IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(
                    back + (if back.contains("?") then "&" else "?") + s"connected=${s.key}"
                  ))))
            }
          case _ => page(ErrorCode.ConnectionCallbackIncomplete)
        }
    }

  private def putConnectionToken(req: Request[IO], id: UUID): IO[Response[IO]] =
    interactive(req)(u =>
      service(id)(s =>
        body[ManualTokenReq](req)(r =>
          conns.saveManual(u.id, s, r.token).flatMap {
            case Left(e) => err(ErrorCode.ConnectionFailed, e)
            case Right(_) =>
              sbirds.observe.Metrics.count("skakapo.connections.created", "service" -> s.key, "via" -> "manual") *>
                NoContent()
          }
        )
      )
    )

  private def deleteConnection(req: Request[IO], id: UUID): IO[Response[IO]] =
    interactive(req)(u => conns.disconnect(u.id, id).ifM(NoContent(), err(ErrorCode.ConnectionNotFound)))

  private def postConnectionRefresh(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(u =>
      service(id)(s =>
        conns.token(u.id, s.id).flatMap {
          case None => err(ErrorCode.NotConnected)
          case Some(t) =>
            conns.forget(u.id, s.id) *> conns.learnTools(s, t).attempt.flatMap {
              case Right(ts) => Ok(Json.obj("tools" -> ts.size.asJson))
              case Left(e) => err(ErrorCode.ToolListFailed, s"ツールを読めませんでした: ${skakapo.core.Text.errMsg(e, 300)}")
            }
        }
      )
    )

  private def service(id: UUID)(f: ServiceDto => IO[Response[IO]]): IO[Response[IO]] =
    config.service(id).flatMap {
      case Some(s) if s.enabled => f(s)
      case _ => err(ErrorCode.ServiceNotFound)
    }

  private def page(code: ErrorCode, detail: String | Null = null): IO[Response[IO]] =
    HtmlPage.error("連携できませんでした", code, "/connections", "連携の画面へ戻る", detail)

}

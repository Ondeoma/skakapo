package skakapo.api

import sbirds.shared.SafePath
import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.auth.{Crypto, LoginResult, OidcClient}
import sbirds.http.{HttpParts, OidcLoginFlow}
import skakapo.auth.ApiKeys
import skakapo.core.ErrorCode
import skakapo.db.*
import skakapo.shared.*

import java.time.Instant
import java.util.UUID

/** ログイン（OIDC・開発用）・ログアウト・自分の情報とアイコン・ユーザーの検索. */
final class AuthRoutes(oidc: Option[OidcClient], users: UserStore, val sessions: SessionAuth, cfg: ApiConfig)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "auth" / "login" => getAuthLogin(req)
    case req @ GET -> Root / "auth" / "callback" => getAuthCallback(req)
    case req @ POST -> Root / "auth" / "logout" => postAuthLogout(req)
    case req @ POST -> Root / "auth" / "dev-login" if cfg.devLogin && oidc.isEmpty => postAuthDevLogin(req)
    case GET -> Root / "api" / "client-config" => getClientConfig()
    case req @ GET -> Root / "api" / "me" => getMe(req)
    case req @ PATCH -> Root / "api" / "me" => patchMe(req)
    case req @ PUT -> Root / "api" / "me" / "avatar" => putMeAvatar(req)
    case req @ DELETE -> Root / "api" / "me" / "avatar" => deleteMeAvatar(req)
    case req @ GET -> Root / "api" / "users" / UuidVar(id) / "avatar" => getUserAvatar(req, id)
    case req @ GET -> Root / "api" / "tokens" => getTokens(req)
    case req @ POST -> Root / "api" / "tokens" => postTokens(req)
    case req @ DELETE -> Root / "api" / "tokens" / UuidVar(id) => deleteToken(req, id)
  }

  private def getAuthLogin(req: Request[IO]): IO[Response[IO]] = OidcLoginFlow.login(flow)(req)

  private def getAuthCallback(req: Request[IO]): IO[Response[IO]] = OidcLoginFlow.callback(flow)(req)

  /** OIDC のログインの流れ（sbirds.http.OidcLoginFlow）に差し込む、skakapo の部分. */
  private val flow = new OidcLoginFlow.Host[LoginResult] {
    val idp = oidc.map(OidcLoginFlow.Idp(_))

    /**
     * ログインを始めたブラウザの印（state の SHA-256）. IdP から戻ったとき、同じブラウザかを確かめる
     * （攻撃者が途中で止めたコールバックの URL を踏ませて、攻撃者のアカウントでログインさせる「ログイン CSRF」を防ぐ）.
     */
    val cookie = OidcLoginFlow.StateCookie("skakapo_login", cfg.cookieSecure, maxAge = 600, hashed = true)
    override def idpErrorParam: Boolean = true

    def returnTo(req: Request[IO]): String = SafePath.of(param(req, "returnTo"))
    def saveState(state: String, verifier: String, nonce: String, returnTo: String): IO[Unit] =
      users.saveLoginState(state, verifier, nonce, returnTo)
    def takeState(state: String): IO[Option[(String, String, String)]] = users.takeLoginState(state)
    def redirect(to: String): IO[Response[IO]] = AuthRoutes.this.redirect(to)

    def withoutIdp(req: Request[IO], back: String): IO[Response[IO]] =
      if cfg.devLogin then AuthRoutes.this.redirect(s"/login?returnTo=${java.net.URLEncoder.encode(back, "UTF-8")}")
      else err(ErrorCode.OidcNotConfigured)

    def failed(req: Request[IO], f: OidcLoginFlow.Failure): IO[Response[IO]] = {
      import OidcLoginFlow.Failure.*
      f match {
        // IdP からの説明は URL で差し替えられるので画面には出さず、ログにだけ残す
        case IdpError(e, d) =>
          page(ErrorCode.LoginRejectedByIdp, logDetail = s"${e.take(100)} ${d.getOrElse("").take(300)}")
        case BrowserMismatch => page(ErrorCode.LoginBrowserMismatch)
        case Expired => page(ErrorCode.LoginExpired)
        // 理由の詳細（検証に失敗した項目など）はログにだけ出す
        case Denied(e) => page(ErrorCode.LoginDenied, e.getMessage)
        case Failed(e) => page(ErrorCode.LoginFailed, logDetail = String.valueOf(e.getMessage))
        case NotConfigured => err(ErrorCode.OidcNotConfigured)
        case ParamsMissing => err(ErrorCode.LoginCodeStateRequired)
      }
    }

    def loggedIn(req: Request[IO], res: LoginResult, back: String): IO[Response[IO]] = {
      val id = res.identity
      users.upsert(
        id.sub,
        id.name,
        id.email,
        id.picture.filter(p => p.startsWith("https://") || p.startsWith("http://")),
        id.groups.exists(cfg.adminGroups.contains),
        id.groups.take(100).map(_.take(200))
      ).flatMap { u =>
        if u.disabled then page(ErrorCode.UserDisabled)
        else AuthRoutes.this.loggedIn(u, Some(res.idToken)).flatMap(c => AuthRoutes.this.redirect(back, Some(c)))
      }
    }
  }

  private def postAuthLogout(req: Request[IO]): IO[Response[IO]] =
    if !fromScreen(req) then err(ErrorCode.ScreenOnly)
    else
      sessions.revoke(req).flatMap {
        (idToken, c) =>
          OidcLoginFlow.logoutRedirect(oidc, idToken, cfg.publicUrl + "/", "/login?loggedOut=1").flatMap { to =>
            Ok(Json.obj("redirect" -> to.asJson)).map(_.addCookie(c))
          }
      }

  private def postAuthDevLogin(req: Request[IO]): IO[Response[IO]] =
    body[Json](req) { j =>
      val c = j.hcursor
      val name = c.get[String]("name").getOrElse("").trim
      if name.isEmpty then err(ErrorCode.NameRequired)
      else
        for {
          // 開発用のログインでは、画面で選んだときだけ管理者にする. groups は IdP のグループの代わり（利用者の種類を試すため）
          u <- users.upsert(
            s"dev:$name",
            name,
            c.get[String]("email").toOption.map(_.trim).filter(_.contains("@")),
            None,
            c.get[Boolean]("admin").getOrElse(false),
            c.get[List[String]]("groups").getOrElse(Nil).take(20).map(_.trim.take(100)).filter(_.nonEmpty)
          )
          ck <- loggedIn(u, None)
          r <-
            Ok(Json.obj("redirect" -> SafePath.of(c.get[String]("returnTo").toOption).asJson)).map(_.addCookie(ck))
        } yield r
    }

  private def getClientConfig(): IO[Response[IO]] =
    Ok(ClientConfigDto(cfg.devLogin && oidc.isEmpty, oidc.isDefined, cfg.version, cfg.agentReady))

  private def getMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u => sessions.appNames(req).flatMap(n => Ok(u.me.copy(appNames = n))))

  private def patchMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u =>
      body[UpdateMeReq](req) { r =>
        val tz = r.timezone.map(_.trim).filter(_.nonEmpty)
        if tz.exists(z => scala.util.Try(java.time.ZoneId.of(z)).isFailure) then
          err(ErrorCode.TimeZoneInvalid)
        else if r.name.exists(_.trim.length > 100) then err(ErrorCode.NameTooLong)
        else users.updateMe(u.id, r.name, tz, r.showDetails) *> me(u)
      }
    )

  private def putMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req) { u =>
      req.body.take(AvatarImage.MaxBytes + 1L).compile.to(Array).flatMap { data =>
        if data.length > AvatarImage.MaxBytes then err(ErrorCode.AvatarTooLarge)
        else
          AvatarImage.detect(data).fold(err(ErrorCode.AvatarTypeUnsupported)) { ct =>
            users.saveAvatar(u.id, ct, data) *> me(u)
          }
      }
    }

  private def deleteMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u => users.deleteAvatar(u.id) *> me(u))

  private def getUserAvatar(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req) { _ =>
      users.avatar(id).flatMap {
        case None => err(ErrorCode.AvatarNotFound)
        case Some((ct, data)) =>
          // JSON の変換が優先されないよう、バイト列のまま返す
          IO.pure(Response[IO](Status.Ok).withBodyStream(fs2.Stream.emits(data)).putHeaders(
            headers.`Content-Type`(MediaType.unsafeParse(ct)),
            "Cache-Control" -> "private, max-age=31536000, immutable",
            "X-Content-Type-Options" -> "nosniff",
            "Content-Security-Policy" -> "default-src 'none'"
          ))
      }
    }

  private def getTokens(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u => users.tokens(u.id).flatMap(Ok(_)))

  private def postTokens(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u =>
      body[ApiTokenReq](req) { r =>
        val name = r.name.trim
        if name.isEmpty || name.length > 100 then err(ErrorCode.ApiTokenNameInvalid)
        else if !TokenScope.all.contains(r.scope) then err(ErrorCode.ApiTokenScopeInvalid)
        else if r.expiresInDays.exists(d => d < 1 || d > 3650) then err(ErrorCode.ApiTokenExpiryInvalid)
        else {
          val (token, prefix) = ApiKeys.generate()
          users.createToken(
            u.id,
            name,
            prefix,
            Crypto.sha256(token),
            r.scope,
            r.expiresInDays.map(d => Instant.now().plusSeconds(d * 86400L))
          )
            .flatMap(t => Created(ApiTokenCreated(t, token)))
        }
      }
    )

  private def deleteToken(req: Request[IO], id: UUID): IO[Response[IO]] =
    interactive(req)(u => users.revokeToken(u.id, id).ifM(NoContent(), err(ErrorCode.ApiTokenNotFound)))

  // SafePath は % を通すので、URL として読めないものは / に戻す
  override protected def redirect(to: String, cookie: Option[ResponseCookie] = None): IO[Response[IO]] =
    IO.pure(HttpParts.redirect(to, cookie, noStore = true, lenient = true))

  /** 変更した後の自分の情報. */
  private def me(u: User): IO[Response[IO]] = users.find(u.id).flatMap(x => Ok(x.getOrElse(u).me))

  /** セッションを発行する. */
  private def loggedIn(u: User, idToken: Option[String]): IO[ResponseCookie] =
    sessions.issue(u, idToken) <*
      sbirds.observe.Metrics.count("skakapo.logins", "via" -> idToken.fold("dev")(_ => "oidc"))

  private def page(code: ErrorCode, detail: String | Null = null, logDetail: String = ""): IO[Response[IO]] =
    HtmlPage.error("ログインできませんでした", code, "/auth/login", "もう一度ログインする", detail, logDetail)

}

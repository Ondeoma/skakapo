package skakapo.agent

import cats.effect.*
import cats.syntax.all.*
import cats.effect.implicits.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import sbirds.auth.Crypto
import skakapo.auth.SecretBox
import sbirds.observe.Log
import skakapo.core.ErrorCode
import skakapo.db.*
import skakapo.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

/**
 * 利用者ごとの連携. 各サービスに OAuth（認可コード + PKCE）でトークンを発行してもらい、暗号化して DB に置く.
 * OAuth に対応していないサービスは、利用者が発行した API トークンを貼り付けてもらう.
 * 連携したときに、そのサービスの MCP のツールの一覧を読んで覚える（システムとして「何ができるか」を把握する）.
 */
final class Connections(config: ConfigStore,
                        store: ConnectionStore,
                        box: Option[SecretBox],
                        http: Client[IO],
                        mcp: Mcp,
                        publicUrl: String) {

  val callbackUrl: String = s"${publicUrl.stripSuffix("/")}/api/connections/callback"

  private def needBox: Either[String, SecretBox] = box.toRight("APP_SECRET が未設定のため、連携のトークンを保存できません（管理者に連絡してください）")

  /**
   * 利用者のトークン（復号したもの）. 期限が切れている（1分以内に切れる）なら、refresh token で更新する.
   * 更新できなければ None（連携し直してもらう）.
   */
  def token(userId: UUID, serviceId: UUID): IO[Option[String]] =
    store.find(userId, serviceId).flatMap(_.flatTraverse(tokenOf))

  /** 利用者の連携をまとめて（サービス → トークン. 期限が切れて更新できなければ None）. */
  def tokens(userId: UUID): IO[Map[UUID, Option[String]]] =
    store.list(userId).flatMap(_.parTraverseN(4)(c => tokenOf(c).map(c.serviceId -> _))).map(_.toMap)

  /** エージェント専用アカウントのトークン（復号したもの. 有効なサービスだけ）. */
  def agentTokens: IO[Map[UUID, String]] =
    config.agentTokens.map(_.flatMap((id, enc) => box.flatMap(_.decrypt(enc)).map(id -> _)))

  /** ツールの一覧を覚えておくときの、エージェント専用アカウントの鍵（利用者の ID の代わり）. */
  val AgentKey: UUID = NilId.value

  private def tokenOf(c: Connection): IO[Option[String]] =
    if c.expiresAt.forall(_.isAfter(Instant.now().plusSeconds(60))) then IO.pure(box.flatMap(_.decrypt(c.token)))
    else refresh(c)

  private def refresh(c: Connection): IO[Option[String]] =
    (box, box.flatMap(b => Option(c.refresh).filter(_.nonEmpty).flatMap(b.decrypt))) match {
      case (Some(b), Some(rt)) =>
        (config.service(c.serviceId), config.serviceSecret(c.serviceId)).flatMapN {
          case (Some(s), enc) if s.enabled && s.oauth.usable =>
            tokenRequest(
              s,
              enc.flatMap(b.decrypt).getOrElse(""),
              List("grant_type" -> "refresh_token", "refresh_token" -> rt)
            ).flatMap {
              // 同じ refresh token で別の実行（別のノード）が先に更新していれば、そのトークンを使う
              case Left(e) => store.find(c.userId, c.serviceId).flatMap {
                  case Some(n) if n.token != c.token && n.expiresAt.forall(_.isAfter(Instant.now().plusSeconds(60))) =>
                    IO.pure(b.decrypt(n.token))
                  case _ => Log.report(ErrorCode.TokenRefreshFailed, s"${s.key}: $e").as(None)
                }
              case Right((access, newRefresh, scope, expires)) =>
                // refresh token が返らなければ、前のものを使い続ける
                store.save(c.copy(
                  token = b.encrypt(access),
                  refresh = newRefresh.fold(c.refresh)(b.encrypt),
                  scope = if scope.nonEmpty then scope else c.scope,
                  expiresAt = expires
                )).as(Some(access))
            }
          case _ => IO.pure(None)
        }
      case _ => IO.pure(None)
    }

  // ---------------------------------------------------------------- OAuth

  /** 連携を始める. IdP（各サービス）の許可の画面の URL を返す. */
  def start(userId: UUID, s: ServiceDto, returnTo: String): IO[Either[String, Uri]] =
    if !s.enabled then IO.pure(Left("このサービスは無効です"))
    else if !s.oauth.usable then IO.pure(Left("このサービスは OAuth の設定がありません（管理者に連絡するか、API トークンで連携してください）"))
    else {
      val state = Crypto.randomToken(); val verifier = Crypto.randomToken(48)
      Uri.fromString(s.oauth.authorizeUrl) match {
        case Left(_) => IO.pure(Left("OAuth の認可の URL が正しくありません"))
        case Right(u) =>
          store.saveState(state, userId, s.id, verifier, returnTo).as(Right(u.withQueryParams(Map(
            "response_type" -> "code",
            "client_id" -> s.oauth.clientId,
            "redirect_uri" -> callbackUrl,
            "state" -> state,
            "code_challenge" -> Crypto.pkceChallenge(verifier),
            "code_challenge_method" -> "S256"
          ) ++
            Option.when(s.oauth.scopes.trim.nonEmpty)("scope" -> s.oauth.scopes.trim))))
      }
    }

  /** 各サービスから戻ってきたところ. 認可コードをトークンに換えて保存する. (サービス, 戻る先). */
  def finish(state: String, code: String, userId: UUID): IO[Either[String, (ServiceDto, String)]] =
    store.takeState(state).flatMap {
      case None => IO.pure(Left("連携の有効期限が切れました。もう一度連携してください"))
      case Some((uid, _, _, _)) if uid != userId => IO.pure(Left("連携を始めた人と違います。もう一度連携してください"))
      case Some((_, sid, verifier, back)) =>
        (config.service(sid), config.serviceSecret(sid)).flatMapN {
          case (None, _) => IO.pure(Left("サービスが見つかりません"))
          case (Some(s), enc) =>
            needBox match {
              case Left(e) => IO.pure(Left(e))
              case Right(b) =>
                val secret = enc.flatMap(b.decrypt).getOrElse("")
                tokenRequest(
                  s,
                  secret,
                  List(
                    "grant_type" -> "authorization_code",
                    "code" -> code,
                    "redirect_uri" -> callbackUrl,
                    "code_verifier" -> verifier
                  )
                ).flatMap {
                  case Left(e) => IO.pure(Left(e))
                  case Right((access, refresh, scope, expires)) =>
                    store.save(Connection(
                      userId,
                      s.id,
                      b.encrypt(access),
                      refresh.fold("")(b.encrypt),
                      scope,
                      "oauth",
                      expires,
                      Instant.now()
                    )) *>
                      learnTools(s, access).attempt.as(Right((s, back)))
                }
            }
        }
    }

  /** トークンの入口に頼む（認可コード・refresh token の共通）. */
  private def tokenRequest(
    s: ServiceDto,
    secret: String,
    grant: List[(String, String)]): IO[Either[String, (String, Option[String], String, Option[Instant])]] = {
    val github = s.kind == ServiceKind.Github
    // sBirds シリーズはクライアントの認証を Basic で受ける. GitHub は本文の client_secret
    val form = UrlForm((grant ++ List("client_id" -> s.oauth.clientId) ++
      (if github then List("client_secret" -> secret) else Nil))*)
    OAuthToken.request(
      http,
      s.oauth.tokenUrl,
      form,
      Option.when(!github)(Authorization(BasicCredentials(s.oauth.clientId, secret)))
    )
      .map(_.map(t => (t.access, t.refresh, t.scope, t.expiresIn.map(Instant.now().plusSeconds(_)))))
  }

  // ---------------------------------------------------------------- 貼り付けたトークン

  /** 利用者が発行した API トークンで連携する（MCP があれば、ツールの一覧を読めるかで確かめる）. */
  def saveManual(userId: UUID, s: ServiceDto, token: String): IO[Either[String, Unit]] = {
    val t = token.trim
    if !s.manualToken then IO.pure(Left("このサービスは API トークンでは連携できません"))
    else if t.isEmpty || t.length > 4000 || t.exists(_.isWhitespace) then IO.pure(Left("トークンを正しく貼り付けてください"))
    else
      needBox match {
        case Left(e) => IO.pure(Left(e))
        case Right(b) =>
          val check = learnTools(s, t).attempt.map(_.leftMap(e => s"トークンで ${s.name} に接続できませんでした: ${mcpErr(e)}").void)
          check.flatMap(_.traverse(_ =>
            store.save(Connection(userId, s.id, b.encrypt(t), "", "", "manual", None, Instant.now()))
          ))
      }
  }

  def disconnect(userId: UUID, serviceId: UUID): IO[Boolean] =
    store.delete(userId, serviceId).map(_.isDefined).flatTap(_ => forget(userId, serviceId))

  /** サービスの送り先が変わったときに、全員の連携を外す（利用者のトークンを新しい送り先へ送らない）. */
  def disconnectAll(serviceId: UUID): IO[Unit] = store.deleteForService(serviceId) *> forgetService(serviceId)

  /** サービスのツールの一覧を、全員分（専用アカウントの分も）忘れる. */
  def forgetService(serviceId: UUID): IO[Unit] = cache.forget(_._2 == serviceId)

  // ---------------------------------------------------------------- ツール

  /** MCP のツールの一覧（MCP がなければ空）. */
  private def list(s: ServiceDto, token: String): IO[List[McpTool]] =
    if s.mcpUrl.isEmpty then IO.pure(Nil) else mcp.listTools(s.mcpUrl, token)

  private def mcpErr(e: Throwable): String = e match {
    case e: McpError => e.message
    case e => s"接続できませんでした（${e.getClass.getSimpleName}）"
  }

  /** トークンでツールの一覧を読めるか（保存する前の確かめ. 覚えはしない）. */
  def probe(s: ServiceDto, token: String): IO[Either[String, Int]] = list(s, token).attempt.map(_.bimap(mcpErr, _.size))

  /** ツールの一覧を読んで、システムとして覚える（サービスとして何ができるか）. */
  def learnTools(s: ServiceDto, token: String): IO[List[McpTool]] =
    list(s, token).flatTap(ts =>
      IO.whenA(s.mcpUrl.nonEmpty)(config.replaceTools(s.id, ts.map(t => t.info -> t.schema)))
    )

  /** 利用者ごとのツールの一覧（5分覚える. 連携を外したら忘れる）. 鍵は (利用者, サービス). */
  private val cache = new TokenCache[(UUID, UUID), List[McpTool]](margin = Duration.Zero)

  /** 実行のときは読むだけ（システムとして覚えるツールは、連携したとき・読み直したときに書く. 利用者ごとの権限の違いで入れ替えない）. */
  def tools(userId: UUID, s: ServiceDto, token: String): IO[List[McpTool]] =
    // 読めなければ例外のまま返す（覚えない）. 読めたら5分覚える
    cache.get(userId -> s.id)((list(s, token), IO.realTime).mapN((ts, now) =>
      Right(ts -> (now + 5.minutes).toMillis)
    )).map(_.getOrElse(Nil))

  def forget(userId: UUID, serviceId: UUID): IO[Unit] = cache.forget(_ == (userId -> serviceId))
}

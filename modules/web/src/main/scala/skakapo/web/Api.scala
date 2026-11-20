package skakapo.web

import io.circe.*
import io.circe.syntax.*
import org.scalajs.dom
import skakapo.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** バックエンドの REST API（呼び出しの共通部分は sbirds.web.ApiBase. 401 はログインへ、エラーは {"error": ...} の文言にする）. */
object Api extends sbirds.web.ApiBase {

  // ---------------------------------------------------------------- 認証・自分

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")
  def devLogin(name: String, email: String, returnTo: String, admin: Boolean = false): Future[Json] =
    post[Json](
      "/auth/dev-login",
      Json.obj("name" -> name.asJson, "email" -> email.asJson, "returnTo" -> returnTo.asJson, "admin" -> admin.asJson)
    )
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def updateMe(r: UpdateMeReq): Future[MeDto] = patch[MeDto]("/api/me", r.asJson)

  /** アイコンを保存する（画像そのものを送る）・消す. */
  def uploadAvatar(image: dom.Blob): Future[MeDto] =
    putRaw[MeDto]("/api/me/avatar", image, Some(image.`type`))
  def deleteAvatar(): Future[MeDto] = request(dom.HttpMethod.DELETE, "/api/me/avatar").map(as[MeDto])

  // ---------------------------------------------------------------- 会話

  def requestTypes(): Future[List[RequestTypeDto]] = get[List[RequestTypeDto]]("/api/request-types")
  def repositories(): Future[List[RepositoryDto]] = get[List[RepositoryDto]]("/api/repositories")
  def sessions(archived: Boolean = false): Future[List[SessionDto]] =
    get[List[SessionDto]](s"/api/sessions?archived=$archived")
  def start(r: StartReq): Future[SessionDto] = post[SessionDto]("/api/sessions", r.asJson)
  def session(id: UUID): Future[EventsDto] = get[EventsDto](s"/api/sessions/$id")

  /** 新しい出来事（wait 秒まで待つ）. */
  def events(id: UUID, after: Long, waitSec: Int): Future[EventsDto] =
    get[EventsDto](s"/api/sessions/$id/events?after=$after&wait=$waitSec")
  def say(id: UUID, text: String, attachments: List[UUID] = Nil): Future[Unit] =
    post[Json](s"/api/sessions/$id/messages", MessageReq(text, attachments).asJson).map(_ => ())
  def cancel(id: UUID): Future[Unit] = post[Json](s"/api/sessions/$id/cancel").map(_ => ())
  def archive(id: UUID, archived: Boolean): Future[Unit] =
    patch[Json](s"/api/sessions/$id", Json.obj("archived" -> archived.asJson)).map(_ => ())
  def deleteSession(id: UUID): Future[Unit] = delete(s"/api/sessions/$id")
  def renameSession(id: UUID, title: String): Future[Unit] =
    patch[Json](s"/api/sessions/$id", Json.obj("title" -> title.asJson)).map(_ => ())

  /** 画像を上げる（会話を始めるときに ID を渡す）. */
  def uploadAttachment(file: dom.Blob, name: String): Future[AttachmentDto] =
    postRaw[AttachmentDto](s"/api/attachments?name=${enc(name)}", file, Some(file.`type`))
  def credit(): Future[CreditDto] = get[CreditDto]("/api/me/credit")
  def adminCredits(): Future[AdminCreditsDto] = get[AdminCreditsDto]("/api/admin/credits")
  def setUserCap(userId: UUID, r: UserCapReq): Future[Unit] =
    put[Json](s"/api/admin/credits/$userId", r.asJson).map(_ => ())
  def userContext(): Future[String] = get[UserContextDto]("/api/me/context").map(_.text)
  def saveUserContext(text: String): Future[String] =
    put[UserContextDto]("/api/me/context", UserContextDto(text).asJson).map(_.text)

  // ---------------------------------------------------------------- 連携

  def connections(): Future[List[ConnectionDto]] = get[List[ConnectionDto]]("/api/connections")

  /** OAuth の許可の画面の URL. */
  def connectOAuth(serviceId: UUID, returnTo: String): Future[String] =
    post[Json](
      s"/api/connections/$serviceId/oauth?returnTo=${enc(returnTo)}"
    ).map(_.hcursor.get[String]("url").getOrElse(""))
  def connectToken(serviceId: UUID, token: String): Future[Unit] =
    put[Json](s"/api/connections/$serviceId/token", ManualTokenReq(token).asJson).map(_ => ())
  def disconnect(serviceId: UUID): Future[Unit] = delete(s"/api/connections/$serviceId")
  def refreshTools(serviceId: UUID): Future[Int] =
    post[Json](s"/api/connections/$serviceId/refresh").map(_.hcursor.get[Int]("tools").getOrElse(0))

  // ---------------------------------------------------------------- 管理

  /** 新しく作る（POST base）か、更新する（PUT base/id）. */
  private def upsert[A: Decoder](base: String, id: Option[UUID], body: Json): Future[A] =
    id.fold(post[A](base, body))(i => put[A](s"$base/$i", body))

  def adminSettings(): Future[AdminSettingsDto] = get[AdminSettingsDto]("/api/admin/settings")
  def saveAdminSettings(r: AdminSettingsReq): Future[AdminSettingsDto] =
    put[AdminSettingsDto]("/api/admin/settings", r.asJson)
  def adminPrompt(): Future[String] = get[Json]("/api/admin/prompt").map(_.hcursor.get[String]("text").getOrElse(""))
  def adminStats(): Future[StatsDto] = get[StatsDto]("/api/admin/stats")
  def adminServices(): Future[List[ServiceDto]] = get[List[ServiceDto]]("/api/admin/services")
  def saveService(id: Option[UUID], r: ServiceReq): Future[ServiceDto] =
    upsert[ServiceDto]("/api/admin/services", id, r.asJson)
  def deleteService(id: UUID): Future[Unit] = delete(s"/api/admin/services/$id")
  def adminRepositories(): Future[List[RepositoryDto]] = get[List[RepositoryDto]]("/api/admin/repositories")
  def saveRepository(id: Option[UUID], r: RepositoryReq): Future[RepositoryDto] =
    upsert[RepositoryDto]("/api/admin/repositories", id, r.asJson)
  def deleteRepository(id: UUID): Future[Unit] = delete(s"/api/admin/repositories/$id")

  def userGroups(): Future[List[UserGroupDto]] = get[List[UserGroupDto]]("/api/admin/user-groups")
  def saveUserGroup(id: Option[UUID], r: UserGroupReq): Future[UserGroupDto] =
    upsert[UserGroupDto]("/api/admin/user-groups", id, r.asJson)
  def deleteUserGroup(id: UUID): Future[Unit] = delete(s"/api/admin/user-groups/$id")
  def adminUsers(): Future[List[AdminUserDto]] = get[List[AdminUserDto]]("/api/admin/users")
  def assignGroup(userId: UUID, groupId: Option[UUID]): Future[Unit] =
    put[Json](s"/api/admin/users/$userId/group", AssignGroupReq(groupId).asJson).map(_ => ())

  def cloudCredentials(): Future[List[CloudCredentialDto]] =
    get[List[CloudCredentialDto]]("/api/admin/cloud-credentials")
  def saveCloudCredential(id: Option[UUID], r: CloudCredentialReq): Future[CloudCredentialDto] =
    upsert[CloudCredentialDto]("/api/admin/cloud-credentials", id, r.asJson)
  def deleteCloudCredential(id: UUID): Future[Unit] = delete(s"/api/admin/cloud-credentials/$id")

  def gitAccounts(): Future[List[GitAccountDto]] = get[List[GitAccountDto]]("/api/admin/git-accounts")
  def saveGitAccount(id: Option[UUID], r: GitAccountReq): Future[GitAccountDto] =
    upsert[GitAccountDto]("/api/admin/git-accounts", id, r.asJson)
  def deleteGitAccount(id: UUID): Future[Unit] = delete(s"/api/admin/git-accounts/$id")
  def adminRequestTypes(): Future[List[RequestTypeDto]] = get[List[RequestTypeDto]]("/api/admin/request-types")
  def saveRequestType(id: Option[UUID], r: RequestTypeReq): Future[RequestTypeDto] =
    upsert[RequestTypeDto]("/api/admin/request-types", id, r.asJson)
  def deleteRequestType(id: UUID): Future[Unit] = delete(s"/api/admin/request-types/$id")

  // ---------------------------------------------------------------- プッシュ通知（PWA）

  /** VAPID の公開鍵. */
  def pushPublicKey(): Future[String] = get[Json]("/api/push").map(_.hcursor.get[String]("publicKey").getOrElse(""))

  /** この端末で受け取る（PushSubscription.toJSON() の形）・やめる. */
  def pushSubscribe(subscription: Json): Future[Unit] = post[Json]("/api/push/subscriptions", subscription).map(_ => ())
  def pushUnsubscribe(endpoint: String): Future[Unit] =
    deleteWith("/api/push/subscriptions", Json.obj("endpoint" -> endpoint.asJson))

}

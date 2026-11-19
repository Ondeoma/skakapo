package skakapo.shared

import io.circe.*
import io.circe.derivation.{Configuration, ConfiguredDecoder}

import java.util.UUID

// バックエンド(JVM)と画面(Scala.js)の両方で使う API の型。
// 日時は ISO 8601 の UTC の文字列（2026-11-15T01:00:00Z）. 画面(Scala.js)で java.time を使わないため、文字列で持つ。

/** 省いた項目は既定の値にする（API・MCP から必要な項目だけ送れるように）. */
given Configuration = Configuration.default.withDefaults

/**
 * ログインや連携の後に戻る先. 同じサイトのパスだけを通す（サーバーと画面で同じ確認をする）.
 * 「//evil」「/\\evil」や、ブラウザが読み飛ばすタブ・改行を挟んだ「/\t/evil」で別のサイトへ飛ばされないよう、使える文字を絞る.
 */
object SafePath {
  private val re = "^/(?![/\\\\])[A-Za-z0-9\\-._~!$&'()*+,;=:@%/?#]*$".r
  def of(
    s: Option[String],
    default: String = "/"
  ): String = s.map(_.trim).filter(re.matches).getOrElse(default)
}

final case class ClientConfigDto(
  devLogin: Boolean,
  oidc: Boolean,
  version: String,
  agentReady: Boolean = false) derives Encoder.AsObject, Decoder

final case class UserBrief(
  id: UUID,
  name: String,
  email: Option[String],
  avatarUrl: Option[String]) derives Encoder.AsObject, Decoder

/** avatarUploaded は、自分で保存したアイコンがあるか（なければ avatarUrl は IdP の画像）. */
/** showDetails: 依頼の画面で、細かい進捗（ツールの呼び出しと結果・作業の状態）を表示するか. */
/** appNames: sbirds のアプリの表示名（slug → 名前. 自身は "_self"）. Cookie のセッションのときだけ入れる. */
final case class MeDto(
  id: UUID,
  name: String,
  email: Option[String],
  avatarUrl: Option[String],
  timezone: String,
  avatarUploaded: Boolean,
  isAdmin: Boolean = false,
  showDetails: Boolean = false,
  appNames: Map[String, String] = Map.empty)
  derives Encoder.AsObject, Decoder {
  def brief: UserBrief = UserBrief(id, name, email, avatarUrl)
}

/** 保存できるアイコン（画像の種類と大きさの上限）. SVG はスクリプトを含められるので受け付けない. */
object AvatarImage {
  val MaxBytes = 1024 * 1024

  /** 先頭のバイトから画像の種類を決める（PNG・JPEG・GIF・WebP 以外は None）. */
  def detect(
    b: Array[Byte]
  ): Option[String] = {
    def at(
      i: Int,
      xs: Int*
    ): Boolean = b.length >= i + xs.size && xs.zipWithIndex.forall(
      (
        x,
        j
      ) => (b(i + j) & 0xff) == x
    )
    if (at(0, 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)) Some("image/png")
    else if (at(0, 0xff, 0xd8, 0xff)) Some("image/jpeg")
    else if (at(0, 0x47, 0x49, 0x46, 0x38)) Some("image/gif")
    else if (at(0, 0x52, 0x49, 0x46, 0x46) && at(8, 0x57, 0x45, 0x42, 0x50)) Some("image/webp")
    else None
  }
}

final case class UpdateMeReq(
  name: Option[String],
  timezone: Option[String],
  showDetails: Option[Boolean] = None) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 依頼の種類（会話の最初のフォーム）

/** フォームの項目の種類. service は連携するサービス、repository は登録したリポジトリから選ぶ. */
object FieldKind {
  val Text = "text"
  val Textarea = "textarea"
  val Select = "select"
  val Service = "service"
  val Repository = "repository"
  val all: List[String] = List(Text, Textarea, Select, Service, Repository)
  def label(
    k: String
  ): String = k match {
    case Text => "1行の文字"
    case Textarea => "複数行の文字"
    case Select => "選択肢"
    case Service => "サービス"
    case Repository => "リポジトリ"
    case _ => k
  }
}

/** フォームの項目. name は英数字（エージェントへの文面・変数の名前）、options は select の選択肢. */
final case class FormField(
  name: String,
  label: String,
  kind: String = FieldKind.Text,
  required: Boolean = false,
  options: List[String] = Nil,
  placeholder: String = "",
  help: String = "") derives Encoder.AsObject, ConfiguredDecoder

final case class RequestTypeDto(
  id: UUID,
  key: String,
  label: String,
  description: String,
  icon: String,
  fields: List[FormField],
  instructions: String,
  position: Int,
  enabled: Boolean) derives Encoder.AsObject, Decoder

final case class RequestTypeReq(
  key: String,
  label: String,
  description: String = "",
  icon: String = "",
  fields: List[FormField] = Nil,
  instructions: String = "",
  position: Int = 0,
  enabled: Boolean = true) derives Encoder.AsObject, ConfiguredDecoder

object FormCheck {
  private val nameRe = "^[a-z][a-z0-9_]{0,31}$".r

  /** 項目の定義の確認（管理画面）. */
  def fields(
    fs: List[FormField]
  ): Either[String, List[FormField]] =
    if (fs.size > 30) Left("項目は30個までです")
    else fs.find(f => !nameRe.matches(f.name)).map(f => Left(s"項目の名前「${f.name}」は英小文字で始まる英小文字・数字・_（32文字まで）にしてください"))
      .orElse(fs.groupBy(_.name).collectFirst { case (n, xs) if xs.size > 1 => Left(s"項目の名前「$n」が重複しています") })
      .orElse(fs.find(f => !FieldKind.all.contains(f.kind)).map(f => Left(s"項目「${f.label}」の種類が正しくありません")))
      .orElse(fs.find(f => f.kind == FieldKind.Select && f.options.isEmpty).map(f =>
        Left(s"項目「${f.label}」の選択肢を入れてください")
      ))
      .orElse(fs.find(f => f.label.trim.isEmpty || f.label.length > 100).map(_ => Left("項目の表示名を100文字以内で入れてください")))
      .getOrElse(Right(fs))

  /** 入力の確認（必須の項目・選択肢）. 前後の空白を除いた値を返す（定義にない項目は捨てる）. */
  def values(
    fs: List[FormField],
    vs: Map[String, String]
  ): Either[String, Map[String, String]] = {
    val clean = fs.flatMap(f => vs.get(f.name).map(v => f.name -> v.trim)).filter(_._2.nonEmpty).toMap
    fs.find(f => f.required && !clean.contains(f.name)).map(f => Left(s"「${f.label}」を入れてください"))
      .orElse(fs.find(f => f.kind == FieldKind.Select && clean.get(f.name).exists(v => !f.options.contains(v))).map(f =>
        Left(s"「${f.label}」は選択肢から選んでください")
      ))
      .orElse(clean.collectFirst { case (k, v) if v.length > 20000 => Left("入力が長すぎます（1項目20000文字まで）") })
      .getOrElse(Right(clean))
  }
}

// ------------------------------------------------------------------ 会話

/** 実行（run）の終わった状態. */
object RunStatus {
  val Queued = "queued"
  val Running = "running"
  val Done = "done"
  val Failed = "failed"
  val Cancelled = "cancelled"
  def label(
    s: String
  ): String = s match {
    case Done => "返事をしました"
    case Cancelled => "止めました"
    case _ => "失敗しました"
  }
}

object SessionStatus {
  val Idle = "idle"
  val Queued = "queued"
  val Running = "running"
  val Failed = "failed"
  def label(
    s: String
  ): String = s match {
    case Queued => "順番待ち"
    case Running => "作業中"
    case Failed => "失敗"
    case _ => "返事待ち"
  }
  def busy(
    s: String
  ): Boolean = s == Queued || s == Running
}

final case class LinkDto(
  label: String,
  url: String) derives Encoder.AsObject, Decoder

final case class SessionDto(
  id: UUID,
  title: String,
  requestType: String,
  requestLabel: String,
  status: String,
  form: Map[String, String],
  links: List[LinkDto],
  createdAt: String,
  updatedAt: String,
  archived: Boolean = false,
  titleAuto: Boolean = false)
  derives Encoder.AsObject, ConfiguredDecoder

/** 会話を始める（フォームの入力・添付したファイルの ID）. 題名（title の項目）を空にすると、エージェントが内容から決める. */
final case class StartReq(
  requestType: String,
  values: Map[String, String],
  attachments: List[UUID] = Nil) derives Encoder.AsObject, ConfiguredDecoder

/** 添付したファイル. */
final case class AttachmentDto(
  id: UUID,
  name: String,
  contentType: String,
  size: Long) derives Encoder.AsObject, Decoder {
  def isImage: Boolean = AttachmentRule.ImageTypes.contains(contentType)
}

/**
 * 添付できるファイル（種類は問わない）. Claude には、画像は画像、PDF は文書、文字のファイルは本文として渡し、それ以外は名前と種類だけを伝える.
 * 会話の添付はやり取りのたびに Claude へ送り直すので、1つの会話の合計にも上限を設ける（Claude の要求の上限 32MB に収める）.
 */
object AttachmentRule {
  val MaxBytes = 10 * 1024 * 1024
  val MaxTotalBytes = 20 * 1024 * 1024
  val MaxCount = 10

  /** Claude が画像として読める形式と大きさ. */
  val ImageTypes: List[String] = List("image/png", "image/jpeg", "image/gif", "image/webp")
  val ImageMaxBytes = 5 * 1024 * 1024
  def mb(
    n: Long
  ): String = s"${n / (1024 * 1024)}MB"

  /** ファイルの大きさを読みやすく. */
  def size(
    n: Long
  ): String =
    if (n < 1024) s"$n B"
    else if (n < 1024 * 1024) s"${(n + 512) / 1024} KB" else f"${n.toDouble / (1024 * 1024)}%.1f MB"

  /** 上げる前の確かめ（画面とサーバーで共通）. */
  def check(
    contentType: String,
    bytes: Long
  ): Either[String, Unit] =
    if (bytes > MaxBytes) Left(s"ファイルは${mb(MaxBytes)}までにしてください")
    else if (ImageTypes.contains(contentType) && bytes > ImageMaxBytes) Left(s"画像は${mb(ImageMaxBytes)}までにしてください")
    else Right(())
}

/** 返事（添付したファイルの ID も付けられる. 文か添付のどちらかは要る）. */
final case class MessageReq(
  text: String,
  attachments: List[UUID] = Nil) derives Encoder.AsObject, ConfiguredDecoder

/** 画面に出す出来事. kind: user / assistant / tool_call / tool_result / status / error / note. data は kind ごとの JSON. */
final case class EventDto(
  id: Long,
  kind: String,
  data: Json,
  at: String) derives Encoder.AsObject, Decoder

final case class EventsDto(
  session: SessionDto,
  events: List[EventDto],
  run: Option[RunDto],
  usage: Option[UsageDto] = None)
  derives Encoder.AsObject, Decoder

/** 会話の使用量（すべての実行の合計. 費用はドル）. */
final case class UsageDto(
  costUsd: Double,
  inputTokens: Long,
  outputTokens: Long,
  cacheReadTokens: Long,
  cacheWriteTokens: Long,
  turns: Int,
  runs: Int)
  derives Encoder.AsObject, Decoder

final case class RunDto(
  id: UUID,
  status: String,
  node: Option[String],
  turns: Int,
  inputTokens: Long,
  outputTokens: Long,
  cacheReadTokens: Long,
  error: Option[String],
  createdAt: String,
  startedAt: Option[String],
  finishedAt: Option[String])
  derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 連携するサービス・リポジトリ（管理画面）

object ServiceKind {
  val Sbirds = "sbirds"
  val Github = "github"
  val Other = "other"
  val all: List[String] = List(Sbirds, Github, Other)

  /** sBirds シリーズの OAuth で求めるスコープの既定（skereru はメールの送信を send として分けている. sruru は管理の基盤なので read だけ）. */
  def sbirdsScopes(
    key: String
  ): String = key match {
    case "skereru" => "read write send"
    case "sruru" => "read"
    case _ => "read write"
  }
  def label(
    k: String
  ): String = k match {
    case Sbirds => "sBirds シリーズ"
    case Github => "GitHub"
    case _ => "そのほか"
  }
}

final case class OAuthConfig(
  authorizeUrl: String = "",
  tokenUrl: String = "",
  clientId: String = "",
  scopes: String = "")
  derives Encoder.AsObject, ConfiguredDecoder {
  def usable: Boolean = authorizeUrl.nonEmpty && tokenUrl.nonEmpty && clientId.nonEmpty
}

final case class ToolInfo(
  name: String,
  description: String,
  readOnly: Option[Boolean] = None) derives Encoder.AsObject, Decoder

final case class ServiceDto(
  id: UUID,
  key: String,
  name: String,
  kind: String,
  baseUrl: String,
  mcpUrl: String,
  description: String,
  notes: String,
  oauth: OAuthConfig,
  hasSecret: Boolean,
  manualToken: Boolean,
  enabled: Boolean,
  position: Int,
  tools: List[ToolInfo] = Nil,
  toolsUpdatedAt: Option[String] = None,
  /** エージェント専用アカウント（そのサービスに作った、エージェントが使うユーザー）. トークンは返さない */
  hasAgentToken: Boolean = false,
  agentAccount: String = "") derives Encoder.AsObject, Decoder

/** secret・agentToken は変えるときだけ（空文字は消す）. agentAccount はエージェント専用アカウントの名前（表示用）. */
final case class ServiceReq(
  key: String,
  name: String,
  kind: String = ServiceKind.Sbirds,
  baseUrl: String = "",
  mcpUrl: String = "",
  description: String = "",
  notes: String = "",
  oauth: OAuthConfig = OAuthConfig(),
  secret: Option[String] = None,
  manualToken: Boolean = true,
  enabled: Boolean = true,
  position: Int = 0,
  agentAccount: String = "",
  agentToken: Option[String] = None)
  derives Encoder.AsObject, ConfiguredDecoder

/** accountId: push・プルリクエストに使う共有アカウント（None なら利用者の GitHub の連携）. */
final case class RepositoryDto(
  id: UUID,
  name: String,
  url: String,
  defaultBranch: String,
  serviceKey: String,
  description: String,
  notes: String,
  enabled: Boolean,
  accountId: Option[UUID] = None) derives Encoder.AsObject, ConfiguredDecoder

final case class RepositoryReq(
  name: String,
  url: String,
  defaultBranch: String = "main",
  serviceKey: String = "",
  description: String = "",
  notes: String = "",
  enabled: Boolean = true,
  accountId: Option[UUID] = None) derives Encoder.AsObject, ConfiguredDecoder

/** Git のホスト（プルリクエストの API の違い）. other は push だけ（プルリクエストは出さない）. */
object GitProvider {
  val Github = "github"
  val Bitbucket = "bitbucket"

  /** Forgejo（Gitea 互換の API. /api/v1）. サーバーごとに場所が違うので、API の URL の既定はリポジトリのホストから作る */
  val Forgejo = "forgejo"
  val Other = "other"
  val all: List[String] = List(Github, Bitbucket, Forgejo, Other)
  def label(
    p: String
  ): String = p match {
    case Github => "GitHub"
    case Bitbucket => "Bitbucket"
    case Forgejo => "Forgejo"
    case _ => "そのほか（push だけ）"
  }
  def defaultApi(
    p: String
  ): String = p match {
    case Github => "https://api.github.com"
    case Bitbucket => "https://api.bitbucket.org/2.0"
    case _ => ""
  }

  /** API の URL の説明（空のときに何を使うか）. */
  def defaultApiHint(
    p: String
  ): String = if (p == Forgejo) "リポジトリのホストの /api/v1" else defaultApi(p)

  /** git の HTTP 認証のユーザー名の既定（トークンだけで認証するとき）. Forgejo はアカウントのユーザー名が要る（既定なし）. */
  def defaultUser(
    p: String
  ): String = p match {
    case Bitbucket => "x-token-auth"
    case Forgejo => ""
    case _ => "x-access-token"
  }

  /** git の HTTP 認証のユーザー名を必ず入れるホスト. */
  def needsUser(
    p: String
  ): Boolean = p == Forgejo
}

/** クラウドの種類と、資格情報の項目（config は秘密でない値、secrets は秘密の値）. */
object CloudProvider {
  val Aws = "aws"
  val Gcp = "gcp"
  val Azure = "azure"
  val all: List[String] = List(Aws, Gcp, Azure)
  def label(
    p: String
  ): String = p match {
    case Aws => "AWS"
    case Gcp => "Google Cloud"
    case Azure => "Azure"
    case _ => p
  }

  /** (項目, 表示名, 必須). allowedHosts はどのクラウドでも使える（送り先をさらに絞る） */
  def configFields(
    p: String
  ): List[(String, String, Boolean)] =
    (p match {
      case Aws => List(("accessKeyId", "アクセスキー ID", true), ("region", "既定のリージョン（例: ap-northeast-1）", true))
      case Gcp => List(("projectId", "既定のプロジェクト ID（空なら鍵の JSON の project_id）", false))
      case Azure => List(
          ("tenantId", "テナント ID", true),
          ("clientId", "クライアント（アプリケーション）ID", true),
          ("subscriptionId", "既定のサブスクリプション ID", false)
        )
      case _ => Nil
    }) :+ ((AllowedHosts, s"送ってよいホスト（任意. カンマ区切りで * を使える. 例: ${hostsExample(p)}. 空なら ${hostsHint(p)}）", false))

  val AllowedHosts = "allowedHosts"

  /** 既定で送れるホストの説明（エラーの文・画面・README で共有）. */
  def hostsHint(
    p: String
  ): String = p match {
    case Aws => "サービスの API（service.amazonaws.com・service.region.amazonaws.com の形）"
    case Gcp => "*.googleapis.com"
    case Azure => "management.azure.com・graph.microsoft.com・*.vault.azure.net・*.core.windows.net・api.loganalytics.io"
    case _ => ""
  }
  private def hostsExample(
    p: String
  ): String = p match {
    case Aws => "ec2.ap-northeast-1.amazonaws.com,logs.*.amazonaws.com"
    case Gcp => "logging.googleapis.com,compute.googleapis.com"
    case _ => "management.azure.com"
  }

  /** 資格情報を使える人. */
  val AccessAdmins = "admins"
  val AccessAll = "all"
  val AccessGroups = "groups"
  val accessAll: List[String] = List(AccessAdmins, AccessGroups, AccessAll)
  def accessLabel(
    a: String
  ): String = a match {
    case AccessAll => "全員の依頼"
    case AccessGroups => "選んだ種類の利用者の依頼"
    case _ => "管理者の依頼だけ"
  }
  def secretFields(
    p: String
  ): List[(String, String, Boolean)] = p match {
    case Aws => List(("secretAccessKey", "シークレットアクセスキー", true), ("sessionToken", "セッショントークン（一時的な認証情報のときだけ）", false))
    case Gcp => List(("serviceAccountJson", "サービスアカウントの鍵（JSON の中身）", true))
    case Azure => List(("clientSecret", "クライアントシークレット", true))
    case _ => Nil
  }
}

/** 利用者の種類（engineer・operator など）. instructions はこの種類の利用者への回答の決まり（エージェントの前提に入れる）. */
final case class UserGroupDto(
  id: UUID,
  key: String,
  label: String,
  description: String,
  instructions: String,
  idpGroups: List[String],
  isDefault: Boolean,
  position: Int) derives Encoder.AsObject, Decoder

final case class UserGroupReq(
  key: String,
  label: String,
  description: String = "",
  instructions: String = "",
  idpGroups: List[String] = Nil,
  isDefault: Boolean = false,
  position: Int = 0) derives Encoder.AsObject, ConfiguredDecoder

object UserGroups {

  /** 利用者の種類を決める: 管理者が付けた種類 → IdP のグループに対応する種類（並び順） → 既定の種類. */
  def resolve(
    assigned: Option[UUID],
    idpGroups: List[String],
    groups: List[UserGroupDto]
  ): Option[UserGroupDto] =
    assigned.flatMap(id => groups.find(_.id == id))
      .orElse(groups.sortBy(_.position).find(_.idpGroups.exists(idpGroups.contains)))
      .orElse(groups.find(_.isDefault))
}

/** 管理画面の利用者の一覧（種類の付け方）. assigned は管理者が付けた種類、effective は実際に使う種類. */
final case class AdminUserDto(
  user: UserBrief,
  isAdmin: Boolean,
  idpGroups: List[String],
  assigned: Option[UUID],
  effective: Option[String])
  derives Encoder.AsObject, Decoder

/** 管理者がその人に種類を付ける（None なら IdP のグループ・既定で決める）. */
final case class AssignGroupReq(
  groupId: Option[UUID] = None) derives Encoder.AsObject, ConfiguredDecoder

/** まだ保存していないもの・利用者でないもの（エージェント専用アカウント）に使う、ゼロの ID. */
object NilId {
  val value: UUID = new UUID(0L, 0L)
}

/** クラウドの資格情報（秘密は返さない. hasSecret だけ）. identity は保存したときに確かめた、認証された相手（AWS の ARN など）. */
final case class CloudCredentialDto(
  id: UUID,
  name: String,
  provider: String,
  config: Map[String, String],
  hasSecret: Boolean,
  notes: String,
  enabled: Boolean,
  identity: Option[String] = None,
  access: String = CloudProvider.AccessAdmins,
  groups: List[UUID] = Nil)
  derives Encoder.AsObject, ConfiguredDecoder {

  /** その利用者の依頼で使えるか（管理者はいつも使える. groups なら、その利用者の種類が選ばれていれば使える）. */
  def usableBy(
    isAdmin: Boolean,
    group: Option[UUID]
  ): Boolean = enabled &&
    (isAdmin || access == CloudProvider.AccessAll ||
      (access == CloudProvider.AccessGroups && group.exists(groups.contains)))
}

/** secrets は秘密の値（変えるときだけ. 新しく作るときは必須. 渡したら全部を置き換える）. */
final case class CloudCredentialReq(
  name: String,
  provider: String,
  config: Map[String, String] = Map.empty,
  secrets: Option[Map[String, String]] = None,
  notes: String = "",
  enabled: Boolean = true,
  access: String = CloudProvider.AccessAdmins,
  groups: List[UUID] = Nil)
  derives Encoder.AsObject, ConfiguredDecoder

/** 共有アカウントの認証の方式. */
object GitAuthKind {

  /** アクセストークン（そのまま使う） */
  val Token = "token"

  /** GitHub App（秘密鍵で署名した JWT から、1時間のインストールのトークンを都度発行する） */
  val GithubApp = "github_app"

  /** Bitbucket の OAuth コンシューマー（client credentials で、2時間のトークンを都度発行する） */
  val BitbucketOAuth = "bitbucket_oauth"
  val all: List[String] = List(Token, GithubApp, BitbucketOAuth)
  def label(
    k: String
  ): String = k match {
    case GithubApp => "GitHub App"
    case BitbucketOAuth => "OAuth コンシューマー"
    case _ => "アクセストークン"
  }

  /** ホストの種類ごとに選べる方式. */
  def forProvider(
    p: String
  ): List[String] = p match {
    case GitProvider.Github => List(Token, GithubApp)
    case GitProvider.Bitbucket => List(Token, BitbucketOAuth)
    case _ => List(Token)
  }
}

/** Git の共有アカウント（秘密は返さない. hasToken だけ）. */
final case class GitAccountDto(
  id: UUID,
  name: String,
  provider: String,
  apiUrl: String,
  username: String,
  hasToken: Boolean,
  authorName: String,
  authorEmail: String,
  authKind: String = GitAuthKind.Token,
  appId: String = "",
  installationId: String = "")
  derives Encoder.AsObject, Decoder

/** token は秘密（アクセストークン・GitHub App の秘密鍵の PEM・OAuth コンシューマーのシークレット）. 変えるときだけ（新しく作るときは必須）. */
final case class GitAccountReq(
  name: String,
  provider: String = GitProvider.Github,
  apiUrl: String = "",
  username: String = "",
  token: Option[String] = None,
  authorName: String = "",
  authorEmail: String = "",
  authKind: String = GitAuthKind.Token,
  appId: String = "",
  installationId: String = "") derives Encoder.AsObject, ConfiguredDecoder

object Keys {
  val service: scala.util.matching.Regex = "^[a-z][a-z0-9]{1,19}$".r
  val repo: scala.util.matching.Regex = "^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$".r
  val requestType: scala.util.matching.Regex = "^[a-z][a-z0-9_-]{0,31}$".r
}

/** 選べるモデル（ID, 表示名）. */
object Models {
  val all: List[(String, String)] = List(
    "claude-opus-5-5" -> "Claude Opus 5.5（既定. 性能と費用のつり合い）",
    "claude-fable-5-2" -> "Claude Fable 5.2（最も高性能. 費用が高い）",
    "claude-sonnet-5-5" -> "Claude Sonnet 5.5（速い・安い）",
    "claude-haiku-5-5" -> "Claude Haiku 5.5（最も速い・安い. 簡単な依頼向け）"
  )
  def label(
    id: String
  ): String = all.find(_._1 == id).map(_._2).getOrElse(id)

  /** 100万トークンあたりの単価（ドル）: (入力, 出力, キャッシュの読み込み). キャッシュの書き込み（1時間）は入力の2倍. */
  private val prices: Map[String, (Double, Double, Double)] = Map(
    "claude-opus-5-5" -> (4.0, 20.0, 0.20),
    "claude-fable-5-2" -> (8.0, 40.0, 0.20),
    "claude-sonnet-5-5" -> (2.0, 10.0, 0.10),
    "claude-haiku-5-5" -> (0.10, 0.50, 0.01)
  )

  /** 1回の応答の費用（ドル）. input はキャッシュを使わなかった入力のトークン. */
  def cost(
    model: String,
    input: Long,
    output: Long,
    cacheRead: Long,
    cacheWrite: Long
  ): Double = {
    val (pi, po, pr) = prices.getOrElse(model, prices("claude-opus-5-5"))
    (input * pi + output * po + cacheRead * pr + cacheWrite * pi * 2) / 1000000.0
  }

  /** 安全のための判断で断られたときに、サーバー側で別のモデルに任せられるか. */
  def fallbacks(
    id: String
  ): Boolean = id != "claude-haiku-5-5"
}

/** エージェントの設定（管理画面の「基本」）. baseContext は CLAUDE.md のように必ず最初に読ませる前提. */
final case class AgentSettings(
  model: String = "claude-opus-5-5",
  effort: String = "high",
  maxTurns: Int = 60,
  maxTokens: Int = 32000,
  baseContext: String = "",
  fallbacks: Boolean = true,
  allowWrites: Boolean = true,
  /** 全体の月の予算（ドル. None なら決めない） */
  monthlyBudgetUsd: Option[Double] = None,
  /** 利用者ごとの月の上限の既定（ドル. None なら上限なし. 人ごとに変えられる） */
  defaultUserCapUsd: Option[Double] = None) derives Encoder.AsObject, ConfiguredDecoder

object Effort {
  val all: List[String] = List("low", "medium", "high", "xhigh", "max")
}

/**
 * 不具合・依頼の起票先（skea のプロジェクトなど. 複数登録できる）. scope はどの依頼をここに起票するか（対象のサービス・内容）.
 * 既定（isDefault）のものは、依頼の種類の手順から {{tracker.service}}・{{tracker.project}} で参照する.
 */
final case class TrackerSettings(
  name: String = "既定",
  service: String = "skea",
  project: String = "",
  scope: String = "",
  notes: String = "",
  isDefault: Boolean = true) derives Encoder.AsObject, ConfiguredDecoder

object Trackers {

  /** 既定の起票先（なければ最初のもの）. */
  def default(
    ts: List[TrackerSettings]
  ): TrackerSettings = ts.find(_.isDefault).orElse(ts.headOption).getOrElse(TrackerSettings())
}

/** 管理画面で編集する設定のまとめ. agentReady は Claude API のキー（ANTHROPIC_API_KEY）が設定されているか. */
final case class AdminSettingsDto(
  agent: AgentSettings,
  trackers: List[TrackerSettings],
  agentReady: Boolean) derives Encoder.AsObject, Decoder
final case class AdminSettingsReq(
  agent: AgentSettings,
  trackers: List[TrackerSettings]) derives Encoder.AsObject, Decoder

/**
 * 今月のクレジット（Claude API の費用. ドル）の使った額と残り. 上限・予算が None なら決めていない（残りも None）.
 * Anthropic のアカウントの残高ではなく、skakapo が実行ごとに計算して積み上げた額.
 */
final case class CreditDto(
  month: String,
  userUsed: Double,
  userCap: Option[Double],
  userRemaining: Option[Double],
  globalUsed: Double,
  globalBudget: Option[Double],
  globalRemaining: Option[Double]) derives Encoder.AsObject, Decoder {

  /** 使えなくなった理由（使えるなら None）. */
  def blocked: Option[String] =
    if (globalRemaining.exists(_ <= 0)) Some(s"今月の全体の予算（${Usd.fmt(globalBudget.getOrElse(0.0))}）を使い切りました。管理者に連絡してください。")
    else if (userRemaining.exists(_ <= 0))
      Some(s"今月のあなたの上限（${Usd.fmt(userCap.getOrElse(0.0))}）に達しました。管理者に上限を上げてもらうか、来月まで待ってください。")
    else None
}

/** 利用者ごとの今月の使った額と上限（管理画面）. capOverride: 既定から変えていれば Some（Some(None) は上限なし）. */
final case class UserCreditDto(
  user: UserBrief,
  used: Double,
  cap: Option[Double],
  capOverride: Option[Option[Double]],
  runs: Int)
  derives Encoder.AsObject, Decoder

/** 管理画面のクレジット: 今月（UTC）の全体の使った額と、利用者ごとの一覧. */
final case class AdminCreditsDto(
  month: String,
  used: Double,
  users: List[UserCreditDto]) derives Encoder.AsObject, Decoder

/** 人ごとの上限を決める. useDefault なら既定に戻す. そうでなければ capUsd（None は上限なし）. */
final case class UserCapReq(
  useDefault: Boolean,
  capUsd: Option[Double] = None) derives Encoder.AsObject, ConfiguredDecoder

object Usd {
  def fmt(
    v: Double
  ): String = {
    val c = math.round(math.max(v, 0) * 100)
    val cents = (c % 100).toString
    "$" + (c / 100).toString + "." + ("0" * (2 - cents.length)) + cents
  }
}

object AttachmentUrl {
  def apply(
    a: AttachmentDto
  ): String = s"/api/attachments/${a.id}"
}

/** 利用者が自分で登録する、エージェントへの前提（自分の担当・好み・よく使うプロジェクトなど）. */
final case class UserContextDto(
  text: String) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 利用者の連携

final case class ConnectionDto(
  service: ServiceDto,
  connected: Boolean,
  via: Option[String],
  scope: String,
  connectedAt: Option[String],
  canOAuth: Boolean) derives Encoder.AsObject, Decoder

final case class ManualTokenReq(
  token: String) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ ノード（冗長構成）

final case class NodeDto(
  name: String,
  version: String,
  slots: Int,
  active: Int,
  startedAt: String,
  seenAt: String,
  alive: Boolean)
  derives Encoder.AsObject, Decoder

final case class StatsDto(
  nodes: List[NodeDto],
  queued: Int,
  running: Int,
  runs24h: Int,
  tokens24h: Long) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ API トークン

/** API トークン. scope は read（参照だけ）か write（変更も）. 日時は UTC の ISO 8601. */
object TokenScope {
  val Read = "read"
  val Write = "write"
  val all: List[String] = List(Read, Write)
  def label(
    s: String
  ): String = if (s == Write) "読み書き" else "参照のみ"
}

final case class ApiTokenDto(
  id: UUID,
  name: String,
  prefix: String,
  scope: String,
  createdAt: String,
  expiresAt: Option[String],
  lastUsedAt: Option[String]) derives Encoder.AsObject, Decoder

final case class ApiTokenReq(
  name: String,
  scope: String,
  expiresInDays: Option[Int]) derives Encoder.AsObject, Decoder

/** 作ったときだけ token（秘密の値）を返す. */
final case class ApiTokenCreated(
  info: ApiTokenDto,
  token: String) derives Encoder.AsObject, Decoder

/** MCP のツールの名前（Claude に見せる名前は「<サービスのキー>__<ツール>」. 64文字まで・英数字と _ - だけ）. */
object ToolNames {
  private val ok = "^[a-zA-Z0-9_-]{1,64}$".r
  val Sep = "__"

  def mcp(
    service: String,
    tool: String
  ): Option[String] = Some(s"$service$Sep$tool").filter(ok.matches)

  /** (サービスのキー, ツール). */
  def split(
    name: String
  ): Option[(String, String)] = name.indexOf(Sep) match {
    case -1 => None
    case i => Some(name.take(i) -> name.drop(i + Sep.length)).filter { case (s, t) => s.nonEmpty && t.nonEmpty }
  }
}

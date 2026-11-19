package skakapo.agent

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.*
import org.http4s.client.Client
import org.typelevel.ci.CIString
import skakapo.shared.{CloudCredentialDto, CloudProvider}

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*

/**
 * クラウドの API を、登録した資格情報で呼ぶ. 資格情報を別のところへ送らないよう、送り先はそのクラウドのホストだけに限る.
 *  - AWS: SigV4 で署名する（*.amazonaws.com）
 *  - GCP: サービスアカウントの鍵で JWT に署名し、1時間のトークンを発行して付ける（*.googleapis.com）
 *  - Azure: サービスプリンシパルの client credentials で、送り先に合うトークンを発行して付ける（management.azure.com・graph.microsoft.com など）
 */
final class CloudClients(http: Client[IO]) {
  private val MaxBody = 2L * 1024 * 1024
  private val tokens = new TokenCache[(java.util.UUID, String), String]

  private def secrets(enc: String): Map[String, String] =
    parse(enc).flatMap(_.as[Map[String, String]]).getOrElse(Map.empty)

  private def hostOf(url: String): Option[(Uri, String)] =
    Uri.fromString(url).toOption.filter(_.scheme.contains(Uri.Scheme.https)).flatMap(u =>
      u.host.map(h => u -> h.value.toLowerCase)
    )
      .filter((u, _) => u.authority.forall(_.userInfo.isEmpty))

  private val awsHost = "^[a-z0-9-]+(\\.[a-z]{2}(-[a-z]+)+-\\d{1,2})?\\.amazonaws\\.com(\\.cn)?$".r

  /** AWS の確かめで分かったアカウント ID（ARN の 5 番目）. S3 に付ける持ち主の指定に使う. */
  val AccountId = "accountId"
  def accountIdOf(arn: String): Option[String] = arn.split(":").lift(4).filter(_.matches("^\\d{12}$"))

  /** Azure の送り先からトークンの scope を決める（決まらない送り先には送らない）. */
  def azureScope(host: String): Option[String] =
    if host == "management.azure.com" then Some("https://management.azure.com/.default")
    else if host == "graph.microsoft.com" then Some("https://graph.microsoft.com/.default")
    else if host.endsWith(".vault.azure.net") then Some("https://vault.azure.net/.default")
    else if List(
        ".blob.core.windows.net",
        ".queue.core.windows.net",
        ".table.core.windows.net",
        ".file.core.windows.net"
      ).exists(host.endsWith)
    then
      Some("https://storage.azure.com/.default")
    else if host == "api.loganalytics.io" then Some("https://api.loganalytics.io/.default")
    else None

  /**
   * 送ってよい送り先か. 既定はそのクラウドの API のホスト. AWS の API Gateway（execute-api）と S3 の静的サイト（s3-website）は
   * だれでも作れるエンドポイントなので、既定では送らない（データの持ち出し先にさせない）. 管理者が allowedHosts でさらに絞れる.
   */
  def allowed(c: CloudCredentialDto, host: String): Boolean = {
    val base = c.provider match {
      // AWS はサービスの API の形（service.amazonaws.com・service.region.amazonaws.com）だけ. bucket.s3…・xxx.execute-api.…・
      // search-xxx.es.… のように、だれでも作れる名前が前に付くホストは通さない
      case CloudProvider.Aws => awsHost.matches(host) && !host.startsWith("s3-website")
      case CloudProvider.Gcp => host.endsWith(".googleapis.com")
      case CloudProvider.Azure => azureScope(host).isDefined
      case _ => false
    }
    val patterns =
      c.config.get(CloudProvider.AllowedHosts).toList.flatMap(_.split(",").map(_.trim.toLowerCase).filter(_.nonEmpty))
    base &&
    (patterns.isEmpty || patterns.exists(p =>
      ("^" + p.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString("[a-z0-9.-]*") + "$").r.matches(host)
    ))
  }

  private def send(req: Request[IO]): IO[(Status, String, String)] =
    http.run(req).use(r =>
      HttpBody.read(r, MaxBody).map(b => (r.status, r.contentType.map(_.mediaType.toString).getOrElse(""), b))
    ).timeout(90.seconds)

  /** 秘密を変えた・消したときに、覚えたトークンを捨てる. */
  def forget(id: java.util.UUID): IO[Unit] = tokens.forget(_._1 == id)

  /** トークン（と期限. ミリ秒）を発行する. */
  private def oauth(url: String, form: UrlForm): IO[Either[String, (String, Long)]] =
    (OAuthToken.request(http, url, form), IO.realTime).mapN((r, now) =>
      r.map(t => t.access -> t.expiresAt(now.toMillis))
    )

  /** GCP: サービスアカウントの鍵（JSON）で JWT に署名して、cloud-platform のトークンを発行する. */
  private def gcpToken(c: CloudCredentialDto, enc: String): IO[Either[String, String]] =
    tokens.get(c.id -> "gcp")(gcpIssue(c, enc))

  /** サービスアカウントの鍵（JSON）: (client_email, private_key, トークンの入口). トークンの入口は oauth2.googleapis.com に限る. */
  private def gcpKey(enc: String): Option[(String, String, String)] = {
    val k = parse(secrets(enc).getOrElse("serviceAccountJson", "")).getOrElse(Json.obj()).hcursor
    (k.get[String]("client_email").toOption, k.get[String]("private_key").toOption).mapN((email, pem) =>
      (
        email,
        pem,
        k.get[String]("token_uri").toOption.filter(_.startsWith("https://oauth2.googleapis.com/")).getOrElse(
          "https://oauth2.googleapis.com/token"
        )
      )
    )
  }

  private def gcpIssue(c: CloudCredentialDto, enc: String): IO[Either[String, (String, Long)]] = gcpKey(enc) match {
    case Some((email, pem, tokenUri)) =>
      IO.realTime.flatMap { t =>
        AppJwt.privateKey(pem).fold(
          e => IO.pure(Left(e)),
          k => {
            val now = t.toSeconds
            oauth(
              tokenUri,
              UrlForm(
                "grant_type" -> "urn:ietf:params:oauth:grant-type:jwt-bearer",
                "assertion" -> AppJwt.rs256(
                  Json.obj(
                    "iss" -> email.asJson,
                    "scope" -> "https://www.googleapis.com/auth/cloud-platform".asJson,
                    "aud" -> tokenUri.asJson,
                    "iat" -> now.asJson,
                    "exp" -> (now + 3600).asJson
                  ),
                  k
                )
              )
            )
          }
        )
      }
    case None => IO.pure(Left("サービスアカウントの鍵（JSON）に client_email と private_key がありません"))
  }

  /** Azure: サービスプリンシパルの client credentials で、scope のトークンを発行する. */
  private def azureToken(c: CloudCredentialDto, enc: String, scope: String): IO[Either[String, String]] =
    tokens.get(c.id -> scope)(azureIssue(c, enc, scope))

  private def azureIssue(c: CloudCredentialDto, enc: String, scope: String): IO[Either[String, (String, Long)]] = {
    val tenant = c.config.getOrElse("tenantId", "")
    if !tenant.matches("^[A-Za-z0-9.-]{1,100}$") then IO.pure(Left("テナント ID が正しくありません"))
    else
      oauth(
        s"https://login.microsoftonline.com/$tenant/oauth2/v2.0/token",
        UrlForm(
          "grant_type" -> "client_credentials",
          "client_id" -> c.config.getOrElse("clientId", ""),
          "client_secret" -> secrets(enc).getOrElse("clientSecret", ""),
          "scope" -> scope
        )
      )
  }

  /** 資格情報で認証できるか（保存する前の確かめ. 覚えたトークンは使わず、入れた秘密で発行し直す）. だれとして認証されたかを返す. */
  def probe(c: CloudCredentialDto, enc: String): IO[Either[String, String]] = c.provider match {
    case CloudProvider.Aws =>
      // 確かめは STS に送る（管理者が送ってよいホストを絞っていても）
      request(
        c.copy(config = c.config - CloudProvider.AllowedHosts),
        enc,
        CloudCall(
          "POST",
          "https://sts.amazonaws.com/",
          Map("content-type" -> "application/x-www-form-urlencoded; charset=utf-8"),
          "Action=GetCallerIdentity&Version=2011-06-15",
          Some("sts"),
          Some("us-east-1")
        )
      ).map { o =>
        if o.isError then Left(s"AWS で認証できませんでした: ${o.text.take(400)}")
        else Right("<Arn>(.*?)</Arn>".r.findFirstMatchIn(o.text).map(_.group(1)).getOrElse("認証できました"))
      }
    case CloudProvider.Gcp => gcpIssue(c, enc).map(_.map(_ => gcpKey(enc).fold("認証できました")(_._1)))
    case CloudProvider.Azure => azureIssue(c, enc, "https://management.azure.com/.default").map(_.map(_ =>
        s"アプリ ${c.config.getOrElse("clientId", "")}"
      ))
    case p => IO.pure(Left(s"クラウドの種類「$p」には対応していません"))
  }

  /** API を呼ぶ. 応答の状態・種類・本文を返す（失敗の状態も、そのまま返してエージェントに読ませる）. */
  def request(c: CloudCredentialDto, enc: String, call: CloudCall): IO[Out] = {
    val method = call.method.trim.toUpperCase
    hostOf(call.url) match {
      case None => IO.pure(Out.err("url は https:// で始まる URL にしてください"))
      case Some((_, host)) if !allowed(c, host) =>
        IO.pure(Out.err(s"資格情報「${c.name}」は、次のホストにだけ送れます: ${c.config.get(CloudProvider.AllowedHosts).filter(_.nonEmpty)
            .getOrElse(CloudProvider.hostsHint(c.provider))}（送ろうとした先: $host）"))
      case Some(_) if !Set("GET", "HEAD", "POST", "PUT", "PATCH", "DELETE").contains(method) =>
        IO.pure(Out.err("method は GET・POST・PUT・PATCH・DELETE・HEAD のどれかにしてください"))
      case Some((uri, host)) =>
        val body = call.body.getBytes(StandardCharsets.UTF_8)
        val userHeaders = call.headers.filterNot((k, _) =>
          Set(
            "authorization",
            "host",
            "x-amz-date",
            "x-amz-security-token",
            "x-amz-content-sha256",
            "x-amz-expected-bucket-owner",
            "content-length"
          ).contains(k.toLowerCase)
        )
        val auth: IO[Either[String, Map[String, String]]] = c.provider match {
          case CloudProvider.Aws =>
            val s = secrets(enc)
            // S3 は自分のアカウントのバケットだけに限る（他人のバケットへ書き出させない）
            val owner = Option.when(
              call.service.contains("s3")
            )(c.config.get(AccountId)).flatten.map("x-amz-expected-bucket-owner" -> _)
            val s3WithoutOwner = call.service.contains("s3") && owner.isEmpty
            (
              call.service.filter(_.matches("^[a-z0-9-]{1,40}$")),
              call.region.orElse(c.config.get("region")).filter(_.matches("^[a-z0-9-]{1,30}$"))
            ) match {
              case _ if s3WithoutOwner =>
                IO.pure(Left(s"資格情報「${c.name}」は S3 を自分のアカウントのバケットに限るためのアカウント ID がありません。管理者に、管理画面で資格情報を保存し直してもらってください"))
              case (Some(svc), Some(region)) =>
                IO.realTimeInstant.map(now =>
                  Right(owner.toMap ++ SigV4.sign(
                    method,
                    uri,
                    userHeaders ++ owner,
                    body,
                    c.config.getOrElse("accessKeyId", ""),
                    s.getOrElse("secretAccessKey", ""),
                    s.get("sessionToken").filter(_.nonEmpty),
                    region,
                    svc,
                    now
                  ))
                )
              case _ => IO.pure(Left("AWS の呼び出しには service（例: ec2・logs・s3）と region（既定は資格情報のリージョン）が要ります"))
            }
          case CloudProvider.Gcp => gcpToken(c, enc).map(_.map(t => Map("authorization" -> s"Bearer $t")))
          case _ => azureScope(host).fold(IO.pure(Left("この送り先には送れません")))(sc =>
              azureToken(c, enc, sc).map(_.map(t => Map("authorization" -> s"Bearer $t")))
            )
        }
        auth.flatMap {
          case Left(e) => IO.pure(Out.err(e))
          case Right(h) =>
            val hs = (userHeaders ++ h).toList.map((k, v) => Header.Raw(CIString(k), v): Header.ToRaw)
            val req = Request[IO](Method.fromString(method).getOrElse(Method.GET), uri).putHeaders(hs*)
            val withBody = if body.isEmpty then req else req.withEntity(body).putHeaders(hs*)
            send(withBody).map((st, ct, text) =>
              Out(
                s"HTTP ${st.code} ${st.reason}${if ct.nonEmpty then s" ($ct)" else ""}\n\n$text",
                isError = !st.isSuccess
              )
            )
              .handleError(e =>
                Out.err(s"呼び出しに失敗しました（${e.getClass.getSimpleName}: ${skakapo.core.Text.errMsg(e, 200)}）")
              )
        }
    }
  }
}

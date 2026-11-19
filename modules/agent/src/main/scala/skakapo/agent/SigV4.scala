package skakapo.agent

import org.http4s.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** AWS Signature Version 4（https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_sigv-create-signed-request.html）. */
object SigV4 {
  private val dateTime = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'").withZone(ZoneOffset.UTC)

  private def hex(
    b: Array[Byte]
  ): String = b.map("%02x".format(_)).mkString
  def sha256(
    b: Array[Byte]
  ): String = hex(MessageDigest.getInstance("SHA-256").digest(b))
  private def hmac(
    key: Array[Byte],
    data: String
  ): Array[Byte] = {
    val m = Mac.getInstance("HmacSHA256"); m.init(new SecretKeySpec(key, "HmacSHA256"));
    m.doFinal(data.getBytes(StandardCharsets.UTF_8))
  }

  /** RFC 3986 の予約されていない文字以外を打ち消す（AWS の決まり）. */
  private def enc(
    s: String
  ): String =
    URLEncoder.encode(s, StandardCharsets.UTF_8).replace("+", "%20").replace("*", "%2A").replace("%7E", "~")

  /**
   * 署名して足すヘッダー（Authorization・X-Amz-Date・必要なら X-Amz-Security-Token・X-Amz-Content-Sha256）.
   * headers は送るヘッダー（content-type・x-amz-* も署名に含める）. パスは渡されたもの（打ち消し済み）をそのまま使う.
   */
  def sign(
    method: String,
    uri: Uri,
    headers: Map[String, String],
    payload: Array[Byte],
    accessKey: String,
    secretKey: String,
    sessionToken: Option[String],
    region: String,
    service: String,
    now: Instant
  ): Map[String, String] = {
    val amzDate = dateTime.format(now)
    val date = amzDate.take(8)
    val payloadHash = sha256(payload)
    val host =
      uri.authority.map(a => a.host.value + a.port.filter(p => p != 443 && p != 80).fold("")(p => s":$p")).getOrElse("")
    val added = Map("host" -> host, "x-amz-date" -> amzDate) ++ sessionToken.map("x-amz-security-token" -> _) ++
      Option.when(service == "s3")("x-amz-content-sha256" -> payloadHash)
    val signed =
      (headers.map(
        (
          k,
          v
        ) => k.toLowerCase -> v.trim
      ).filter(
        (
          k,
          _
        ) => k == "content-type" || k.startsWith("x-amz-")
      ) ++ added).toList.sortBy(_._1)
    val query = uri.query.pairs.map(
      (
        k,
        v
      ) => enc(k) -> enc(v.getOrElse(""))
    ).sortBy(identity).map(
      (
        k,
        v
      ) => s"$k=$v"
    ).mkString("&")
    // パスは打ち消し済みのものを受け取る. S3 以外は、各セグメントをもう一度打ち消す（AWS の決まり）
    val path0 = Option(uri.path.renderString).filter(_.nonEmpty).getOrElse("/")
    val path = if service == "s3" then path0 else path0.split("/", -1).map(enc).mkString("/")
    val canonical = List(
      method.toUpperCase,
      path,
      query,
      signed.map(
        (
          k,
          v
        ) => s"$k:$v\n"
      ).mkString,
      signed.map(_._1).mkString(";"),
      payloadHash
    ).mkString("\n")
    val scope = s"$date/$region/$service/aws4_request"
    val toSign = s"AWS4-HMAC-SHA256\n$amzDate\n$scope\n${sha256(canonical.getBytes(StandardCharsets.UTF_8))}"
    val key =
      List(date, region, service, "aws4_request").foldLeft(s"AWS4$secretKey".getBytes(StandardCharsets.UTF_8))(hmac)
    val signature = hex(hmac(key, toSign))
    (added - "host") +
      ("authorization" ->
        s"AWS4-HMAC-SHA256 Credential=$accessKey/$scope, SignedHeaders=${signed.map(_._1).mkString(";")}, Signature=$signature")
  }
}

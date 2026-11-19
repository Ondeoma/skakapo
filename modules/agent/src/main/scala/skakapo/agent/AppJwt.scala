package skakapo.agent

import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*

import java.nio.charset.StandardCharsets
import java.security.{KeyFactory, PrivateKey, Signature}
import java.security.spec.PKCS8EncodedKeySpec
import java.util.Base64

/** RS256 の JWT（GitHub App・GCP のサービスアカウント）. 秘密鍵は PKCS#1（BEGIN RSA PRIVATE KEY）と PKCS#8（BEGIN PRIVATE KEY）のどちらでもよい. */
object AppJwt {
  private def b64url(
    b: Array[Byte]
  ): String = Base64.getUrlEncoder.withoutPadding.encodeToString(b)

  /** PEM を秘密鍵にする（読めなければ Left）. */
  def privateKey(
    pem: String
  ): Either[String, PrivateKey] = {
    val body = pem.linesIterator.map(_.trim).filterNot(l => l.startsWith("-----") || l.isEmpty).mkString
    val pkcs1 = pem.contains("BEGIN RSA PRIVATE KEY")
    Either.catchNonFatal {
      val der = Base64.getDecoder.decode(body)
      KeyFactory.getInstance("RSA").generatePrivate(new PKCS8EncodedKeySpec(if pkcs1 then pkcs1ToPkcs8(der) else der))
    }.leftMap(_ => "秘密鍵（PEM）を読めません。作った鍵の中身をそのまま貼り付けてください")
  }

  /** PKCS#1 の RSA 秘密鍵を PKCS#8 で包む（Java の標準は PKCS#8 しか読めない）. */
  private def pkcs1ToPkcs8(
    pkcs1: Array[Byte]
  ): Array[Byte] = {
    def len(
      n: Int
    ): Array[Byte] =
      if n < 0x80 then Array(n.toByte)
      else {
        val bs = BigInt(n).toByteArray.dropWhile(_ == 0)
        (0x80 | bs.length).toByte +: bs
      }
    def tlv(
      tag: Int,
      v: Array[Byte]
    ): Array[Byte] = (tag.toByte +: len(v.length)) ++ v
    val rsaAlg = Array[Byte](
      0x30,
      0x0d,
      0x06,
      0x09,
      0x2a,
      0x86.toByte,
      0x48,
      0x86.toByte,
      0xf7.toByte,
      0x0d,
      0x01,
      0x01,
      0x01,
      0x05,
      0x00
    )
    tlv(0x30, Array[Byte](0x02, 0x01, 0x00) ++ rsaAlg ++ tlv(0x04, pkcs1))
  }

  /** 任意のクレームの RS256 の JWT（GitHub App・GCP のサービスアカウントで共用）. */
  def rs256(
    claims: Json,
    key: PrivateKey
  ): String = {
    val head = b64url("""{"alg":"RS256","typ":"JWT"}""".getBytes(StandardCharsets.UTF_8))
    val body = b64url(claims.noSpaces.getBytes(StandardCharsets.UTF_8))
    val sig = Signature.getInstance("SHA256withRSA")
    sig.initSign(key)
    sig.update(s"$head.$body".getBytes(StandardCharsets.US_ASCII))
    s"$head.$body.${b64url(sig.sign())}"
  }

  /** GitHub App の JWT（iss は App ID か Client ID）. 時計のずれに備えて iat は60秒前、有効は9分（GitHub の上限は10分）. */
  def sign(
    iss: String,
    key: PrivateKey,
    nowSec: Long
  ): String =
    rs256(Json.obj("iat" -> (nowSec - 60).asJson, "exp" -> (nowSec + 540).asJson, "iss" -> iss.asJson), key)
}

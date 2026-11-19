package skakapo.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import skakapo.api.*
import sbirds.auth.OidcConfig
import skakapo.db.*

import scala.concurrent.duration.*

/** 設定は環境変数から読む. */
object Config {
  private def opt(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)
  private def int(name: String): Option[Int] = opt(name).flatMap(_.toIntOption)
  private def enabled(name: String): Boolean = opt(name).exists(_.equalsIgnoreCase("true"))
  private def required(name: String): Either[String, String] = opt(name).toRight(s"環境変数 $name を設定してください")
  private def list(name: String): List[String] =
    opt(name).toList.flatMap(_.split(Array(',', ' ')).map(_.trim).filter(_.nonEmpty))

  private def local(url: String): Boolean =
    scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)
      .exists(h => Set("localhost", "127.0.0.1", "[::1]").contains(h) || h.endsWith(".localhost"))

  /** ノードの名前（既定はホスト名. コンテナではコンテナの ID になる）. */
  private def hostName: String = scala.util.Try(java.net.InetAddress.getLocalHost.getHostName).getOrElse("node")

  def load: IO[AppConfig] = IO.fromEither(
    (for {
      dbUrl <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub <- required("PUBLIC_URL").map(_.stripSuffix("/"))
      devLogin = enabled("DEV_LOGIN")
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match {
        // グループを絞らないと IdP のだれでもログインでき、エージェント（Claude API の費用・各サービスへの操作）を使える. 明示したときだけ許す
        case (Some(_), Some(_), Some(_)) if list("OIDC_ALLOWED_GROUPS").isEmpty && !enabled("OIDC_ALLOW_ANY_USER") =>
          Left("OIDC_ALLOWED_GROUPS でログインできるグループを絞ってください（IdP のだれでもログインしてよいときは OIDC_ALLOW_ANY_USER=true）")
        case (Some(i), Some(c), Some(s)) =>
          Right(Some(OidcConfig(
            i,
            c,
            s,
            s"$pub/auth/callback",
            opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
            Some(list("OIDC_SCOPES")).filter(_.nonEmpty).getOrElse(List("openid", "profile", "email")),
            list("OIDC_ALLOWED_GROUPS").toSet
          )))
        // 開発用のログインは名前だけでだれにでもなれるので、手元（localhost）で開くときだけ
        case (None, None, None) if devLogin && !local(pub) => Left("DEV_LOGIN は PUBLIC_URL が localhost のときだけ使えます")
        case (None, None, None) if devLogin => Right(None)
        case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      }
      proxies <- list("TRUSTED_PROXIES").traverse(s =>
        RateLimitConfig.proxy(s).toRight(s"TRUSTED_PROXIES の「$s」は IP か CIDR で書いてください")
      )
      secret <- opt("APP_SECRET") match {
        case Some(s) if s.length < 16 => Left("APP_SECRET は16文字以上にしてください")
        case s => Right(s)
      }
      // このノードで同時に動かす実行の数（エージェントは重いので、マシンの大きさに合わせる）
      slots <- int("WORKER_SLOTS").getOrElse(2) match {
        case n if n >= 0 && n <= 64 => Right(n)
        case _ => Left("WORKER_SLOTS は 0〜64 にしてください（0 は画面と API だけのノード）")
      }
      // 待ち受けるアドレス（既定 0.0.0.0. 開発用のログインのときは 127.0.0.1）
      bind <- opt("BIND_HOST").fold[Either[String, com.comcast.ip4s.Host]](Right(if devLogin then ipv4"127.0.0.1"
      else ipv4"0.0.0.0"))(h =>
        com.comcast.ip4s.Host.fromString(h).toRight("BIND_HOST は IP アドレスかホスト名で書いてください")
      )
      // 開発用のログイン（名前だけで管理者にもなれる）は、手元からしか届かないようにする
      // （コンテナーの中でロードバランサーの後ろに置くときなどは、DEV_LOGIN_ALLOW_REMOTE=true で明示する）
      _ <- Either.cond(
        !devLogin || oidc.isDefined || opt("DEV_LOGIN_ALLOW_REMOTE").contains("true") ||
          Set("127.0.0.1", "::1", "localhost").contains(bind.toString),
        (),
        "DEV_LOGIN のときは BIND_HOST を 127.0.0.1 にしてください（開発用のログインを外に出さないため。" +
          "ロードバランサーの後ろに置くなど、外から直接は届かないなら DEV_LOGIN_ALLOW_REMOTE=true）"
      )
    } yield AppConfig(
      port = int("HTTP_PORT").getOrElse(8080),
      db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(slots * 2 + 8)),
      oidc = oidc,
      api = ApiConfig(
        cookieSecure = pub.startsWith("https://"),
        publicUrl = pub,
        sessionTtl = int("SESSION_DAYS").getOrElse(14).days,
        devLogin = devLogin,
        adminGroups = list("ADMIN_GROUPS").toSet,
        agentReady = sys.env.get("ANTHROPIC_API_KEY").exists(_.trim.nonEmpty),
        rateLimit =
          RateLimitConfig(int("RATE_LIMIT_AUTH").getOrElse(20), int("RATE_LIMIT_PUBLIC").getOrElse(120), proxies)
      ),
      staticDir = opt("STATIC_DIR"),
      appSecret = secret,
      node = opt("NODE_NAME").getOrElse(hostName).take(100),
      slots = slots,
      workDir = opt("WORK_DIR").getOrElse("work"),
      bindHost = bind
    )).left.map(new IllegalArgumentException(_))
  )
}

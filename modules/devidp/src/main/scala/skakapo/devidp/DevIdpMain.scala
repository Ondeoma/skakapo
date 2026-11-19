package skakapo.devidp

import cats.effect.*

/** 開発・テスト用の簡易 OIDC プロバイダ（中身は sbirds-lib. 本番では使わない）. */
object DevIdpMain extends IOApp.Simple {
  def run: IO[Unit] = sbirds.devidp.DevIdp.serve(9098, "skakapo", "skakapo-dev-secret-0123456789")
}

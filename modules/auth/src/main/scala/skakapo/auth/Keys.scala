package skakapo.auth

/** skakapo の API トークン（見分けやすいよう skakapo_ で始める）. */
object ApiKeys extends sbirds.auth.ApiKeys("skakapo_")

/** skakapo の秘密の値の暗号化（鍵の salt は skakapo 固有. 変えると保存済みの値を復号できなくなる）. */
object SecretBox {
  def apply(secret: String): sbirds.auth.SecretBox = new sbirds.auth.SecretBox(secret, "skakapo-secret-box:")
}

type SecretBox = sbirds.auth.SecretBox

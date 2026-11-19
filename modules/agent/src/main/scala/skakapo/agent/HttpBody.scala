package skakapo.agent

import cats.effect.IO
import org.http4s.Response

/** 外部の応答の本文を、上限までだけ読む（大きすぎる応答でメモリを使い切らないため）. */
object HttpBody {
  def read(r: Response[IO], maxBytes: Long): IO[String] =
    r.body.take(maxBytes).through(fs2.text.utf8.decode).compile.string
}

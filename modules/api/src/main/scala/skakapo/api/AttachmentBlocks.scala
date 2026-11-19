package skakapo.api

import com.anthropic.models.messages.{
  Base64ImageSource, ContentBlockParam, DocumentBlockParam, ImageBlockParam, TextBlockParam
}
import skakapo.shared.{AttachmentDto, AttachmentRule}

import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.util.Base64

/**
 * 添付したファイルを Claude に渡す形にする（会話を始めるとき・返事を送るときで共通）.
 *  - 画像（PNG・JPEG・GIF・WebP）: 画像
 *  - PDF: 文書
 *  - 文字のファイル（UTF-8 として読めるもの）: 文書（長ければ先頭だけ）
 *  - それ以外: 名前・種類・大きさだけを伝える（中身は読めない）
 * 先頭に添付の一覧を置き、どのブロックがどのファイルかを分かるようにする.
 */
object AttachmentBlocks {

  /** 文字のファイルを渡す長さ（文字数）. これより長ければ先頭だけを渡す. */
  val MaxTextChars = 200_000

  enum Kind {
    case Image, Pdf, Text, Other
  }

  def kind(a: AttachmentDto, data: Array[Byte]): Kind =
    if a.isImage && data.length <= AttachmentRule.ImageMaxBytes then Kind.Image
    else if isPdf(data) then Kind.Pdf
    else if utf8(data).isDefined then Kind.Text
    else Kind.Other

  def isPdf(data: Array[Byte]): Boolean =
    data.length >= 5 && new String(data, 0, 5, StandardCharsets.US_ASCII) == "%PDF-"

  /** UTF-8 として読めて、制御文字（NUL など. 改行・タブ・改ページを除く）を含まなければ、その文字列. */
  def utf8(data: Array[Byte]): Option[String] =
    try {
      val s = StandardCharsets.UTF_8.newDecoder().onMalformedInput(
        CodingErrorAction.REPORT
      ).onUnmappableCharacter(CodingErrorAction.REPORT)
        .decode(ByteBuffer.wrap(data)).toString
      Option.unless(s.exists(c => c < ' ' && c != '\n' && c != '\r' && c != '\t' && c != '\f'))(s.stripPrefix("﻿"))
    } catch case _: CharacterCodingException => None

  private def label(k: Kind): String = k match {
    case Kind.Image => "画像として添付"
    case Kind.Pdf => "PDF の文書として添付"
    case Kind.Text => "文字のファイルとして添付"
    case Kind.Other => "この種類は中身を読めません"
  }

  private def text(s: String) = ContentBlockParam.ofText(TextBlockParam.builder().text(s).build())

  def apply(atts: List[(AttachmentDto, Array[Byte])]): List[ContentBlockParam] =
    if atts.isEmpty then Nil
    else {
      val kinds = atts.map((a, d) => (a, d, kind(a, d)))
      val list = kinds.map((a, _, k) =>
        s"- ${a.name}（${if a.contentType.isEmpty then "種類不明" else a.contentType}・${AttachmentRule.size(a.size)}）: ${label(k)}"
      )
      text(("利用者がファイルを添付しました。" :: list).mkString("\n")) :: kinds.flatMap {
        case (a, d, Kind.Image) =>
          List(ContentBlockParam.ofImage(ImageBlockParam.builder().source(Base64ImageSource.builder()
            .mediaType(
              Base64ImageSource.MediaType.of(a.contentType)
            ).data(Base64.getEncoder.encodeToString(d)).build()).build()))
        case (a, d, Kind.Pdf) =>
          List(ContentBlockParam.ofDocument(
            DocumentBlockParam.builder().base64Source(Base64.getEncoder.encodeToString(d)).title(a.name).build()
          ))
        case (a, d, Kind.Text) =>
          val s = utf8(d).getOrElse("")
          val body = if s.length <= MaxTextChars then s
          else s.take(MaxTextChars) + s"\n\n（長いので、ここまでの ${MaxTextChars} 文字だけを渡しています。全体は ${s.length} 文字）"
          List(ContentBlockParam.ofDocument(DocumentBlockParam.builder().textSource(if body.isEmpty then "（空のファイル）"
          else body).title(a.name).build()))
        case (_, _, Kind.Other) => Nil
      }
    }
}

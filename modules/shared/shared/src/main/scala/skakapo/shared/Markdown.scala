package skakapo.shared

import scala.collection.mutable

/**
 * エージェントの返事（Markdown）を HTML にする（画面の v-html で出す）.
 * 安全のため、文字はすべて先に HTML の記号を打ち消してから、決まった形だけを要素にする. リンクは http(s) だけ.
 * 対応: 見出し・段落・箇条書き（入れ子・チェックボックス）・番号つきの箇条書き・コードの塊（言語の表示）・表（列の寄せ）・引用・区切り線、
 *       インラインのコード・太字・斜体・打ち消し線・リンク・URL の自動リンク.
 * Scala.js（ES2015）でも動くよう、正規表現の後読みは使わない.
 */
object Markdown {

  def esc(s: String): String = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;").replace("'", "&#39;")

  // ---------------------------------------------------------------- インライン

  private val linkRe   = "\\[([^\\]\\[]+)\\]\\((https?://[^)\\s]+)\\)".r
  // 打ち消した記号（&gt; など. <https://…> や "https://…" の囲み）・末尾の句読点・閉じかっこ・** は URL に含めない（先読みは使える）
  private val notEnt   = "(?!&(?:gt|lt|quot|#39);)"
  private val urlRe    = s"(https?://(?:$notEnt[^\\s<])*?$notEnt[^\\s<.,;:!?)）」。、*])(?=[.,;:!?)）」。、*]*(?:\\s|<|&(?:gt|lt|quot|#39);|$$))".r
  private val boldRe   = "\\*\\*([^*]+)\\*\\*".r
  private val italicRe = "(^|[^*\\w])\\*([^*\\s][^*]*?)\\*(?![*\\w])".r
  private val strikeRe = "~~([^~]+)~~".r

  private def a(href: String, label: String) = s"""<a href="$href" target="_blank" rel="noopener noreferrer">$label</a>"""
  private def q(s: String) = java.util.regex.Matcher.quoteReplacement(s)

  /**
   * 1行の中. コードの中は飾らない. 太字などがコードをまたいでもよいよう、先にコードを目印（\u0001番号\u0002）に置き換えてから
   * 行全体に書式をかけ、最後にコードを戻す.
   */
  def fmtInline(raw: String): String = if (raw.length > MaxLine) esc(raw) else {
    val parts = raw.split("`", -1).toList
    val codes = mutable.ArrayBuffer[String]()
    val text = parts.zipWithIndex.map { case (p, i) =>
      if (i % 2 == 1 && i < parts.size - 1) { codes += s"<code>${esc(p)}</code>"; s"\u0001${codes.size - 1}\u0002" }
      else esc(p.replace("\u0001", "").replace("\u0002", ""))
    }.mkString
    val links = linkRe.replaceAllIn(text, m => q(a(m.group(2), m.group(1))))
    // リンクを作った行（<a ）は、その中の URL を二重にリンクにしないよう自動リンクはしない
    val bare = if (links.contains("<a ")) links else urlRe.replaceAllIn(links, m => q(a(m.group(1), m.group(1))))
    val b = boldRe.replaceAllIn(bare, m => q(s"<strong>${m.group(1)}</strong>"))
    val it = italicRe.replaceAllIn(b, m => q(s"${m.group(1)}<em>${m.group(2)}</em>"))
    "\u0001(\\d+)\u0002".r.replaceAllIn(strikeRe.replaceAllIn(it, m => q(s"<del>${m.group(1)}</del>")), m => q(codes(m.group(1).toInt)))
  }

  // ---------------------------------------------------------------- ブロック

  private val fenceRe  = "^\\s*(```|~~~)\\s*([A-Za-z0-9_+#.-]*).*$".r
  // 見出しは # の数と本文だけを取り出す（末尾の閉じの # は heading() で落とす. 後戻りの多い正規表現にしない）
  private val headRe   = "^ {0,3}(#{1,6})[ \\t]+(.*)$".r
  private val hrRe     = "^\\s{0,3}([-*_])(\\s*\\1){2,}\\s*$".r
  private val ulRe     = "^(\\s*)[-*+]\\s+(.*)$".r
  private val olRe     = "^(\\s*)(\\d{1,9})[.)]\\s+(.*)$".r
  private val quoteRe  = "^\\s{0,3}>\\s?(.*)$".r
  private val taskRe   = "^\\[([ xX])\\]\\s+(.*)$".r
  private val sepCell  = "^\\s*:?-{2,}:?\\s*$".r

  private def cells(line: String): List[String] = {
    val t = line.trim.stripPrefix("|")
    val body = if (t.endsWith("|") && !t.endsWith("\\|")) t.dropRight(1) else t
    // \| は列の区切りにしない
    body.replace("\\|", "\u0000").split("\\|", -1).toList.map(_.replace("\u0000", "|").trim)
  }
  private def isTableSep(line: String): Boolean = line.contains("-") && line.contains("|") && cells(line).forall(c => sepCell.pattern.matcher(c).matches())
  private def isTableRow(line: String): Boolean = line.contains("|") && line.trim.nonEmpty

  private def align(sep: String): String = {
    val c = sep.trim
    if (c.startsWith(":") && c.endsWith(":")) " style=\"text-align:center\""
    else if (c.endsWith(":")) " style=\"text-align:right\""
    else ""
  }

  /** 見出しの本文: 末尾の空白と、空白の後ろの閉じの # を落とす（「C#」の # は残す）. */
  private def heading(body: String): String = {
    val t = body.trim
    val noHashes = t.reverse.dropWhile(_ == '#').reverse
    if (noHashes.length < t.length && (noHashes.isEmpty || noHashes.last.isWhitespace)) noHashes.trim else t
  }

  /** 1行がこれより長ければ、書式を見ずに（打ち消しただけで）段落として出す（とても長い行で重くならないように）. */
  private val MaxLine = 10000
  /** 表の列と行の上限（出力が膨らみすぎないように. 超えた分は出さない）. */
  private val MaxCols = 50
  private val MaxRows = 2000
  /** 引用の入れ子の深さの上限（深すぎる入れ子でスタックがあふれないように）. */
  private val MaxDepth = 8

  /** 箇条書きの項目（入れ子は字下げで決める）. */
  private final case class Item(indent: Int, ordered: Boolean, start: Int, text: String)

  private def renderList(items: List[Item]): String = {
    val out = new StringBuilder
    // (字下げ, 番号つき) の積み重ね
    val stack = mutable.Stack[(Int, Boolean)]()
    def open(it: Item): Unit = {
      out ++= (if (it.ordered) (if (it.start != 1) s"""<ol start="${it.start}">""" else "<ol>") else "<ul>")
      stack.push(it.indent -> it.ordered)
    }
    def close(): Unit = { val (_, o) = stack.pop(); out ++= "</li>" + (if (o) "</ol>" else "</ul>") }
    items.foreach { it =>
      if (stack.isEmpty) open(it)
      else if (it.indent > stack.top._1) open(it)
      else {
        while (stack.nonEmpty && it.indent < stack.top._1) close()
        if (stack.isEmpty) open(it)
        else if (stack.top._2 != it.ordered) { close(); open(it) }
        else out ++= "</li>"
      }
      val body = it.text match {
        case taskRe(mark, rest) =>
          s"""<input type="checkbox" disabled${if (mark != " ") " checked" else ""}> ${fmtInline(rest)}"""
        case t => fmtInline(t)
      }
      out ++= s"""<li${if (taskRe.pattern.matcher(it.text).matches()) " class=\"task\"" else ""}>$body"""
    }
    while (stack.nonEmpty) close()
    out.toString
  }

  def toHtml(md: String): String = render(md, 0)

  private def render(md: String, depth: Int): String = {
    // 改行のたぐい（\r・U+0085・U+2028・U+2029）は \n にそろえる（正規表現の . や $ が行の途中で止まらないように）
    val lines = md.replace("\r\n", "\n").replaceAll("[\r\u0085\u2028\u2029]", "\n").split("\n", -1).toVector
    val out = new StringBuilder
    var i = 0
    val para = mutable.ListBuffer[String]()
    def flushPara(): Unit = if (para.nonEmpty) { out ++= s"<p>${para.map(fmtInline).mkString("<br>")}</p>"; para.clear() }

    while (i < lines.size) {
      val line = lines(i)
      line match {
        case l if l.length > MaxLine =>
          flushPara(); out ++= s"<p>${esc(l)}</p>"; i += 1
        case fenceRe(fence, lang) =>
          flushPara()
          val body = mutable.ListBuffer[String]()
          i += 1
          while (i < lines.size && !lines(i).trim.startsWith(fence)) { body += lines(i); i += 1 }
          val label = if (lang.nonEmpty) s"""<span class="lang">${esc(lang)}</span>""" else ""
          out ++= s"""<pre>$label<code>${esc(body.mkString("\n"))}</code></pre>"""
          i += 1
        case l if l.trim.isEmpty =>
          flushPara(); i += 1
        case headRe(hs, body) =>
          flushPara()
          // 返事の見出しは画面の見出しより1段小さくする（h3〜h6）
          val n = math.min(hs.length + 2, 6)
          out ++= s"<h$n>${fmtInline(heading(body))}</h$n>"; i += 1
        case hrRe(_, _) =>
          flushPara(); out ++= "<hr>"; i += 1
        case l if isTableRow(l) && i + 1 < lines.size && isTableSep(lines(i + 1)) =>
          flushPara()
          val head = cells(l).take(MaxCols)
          val aligns = cells(lines(i + 1)).map(align)
          def td(tag: String, cs: List[String]) = head.indices.map(k => s"<$tag${aligns.lift(k).getOrElse("")}>${fmtInline(cs.lift(k).getOrElse(""))}</$tag>").mkString
          out ++= s"""<div class="table-scroll"><table><thead><tr>${td("th", head)}</tr></thead><tbody>"""
          i += 2
          var rows = 0
          while (i < lines.size && isTableRow(lines(i))) {
            if (rows < MaxRows) out ++= s"<tr>${td("td", cells(lines(i)))}</tr>"
            rows += 1; i += 1
          }
          out ++= "</tbody></table></div>"
        case quoteRe(_) if depth < MaxDepth =>
          flushPara()
          val inner = mutable.ListBuffer[String]()
          while (i < lines.size && quoteRe.pattern.matcher(lines(i)).matches()) { inner += quoteRe.findFirstMatchIn(lines(i)).map(_.group(1)).getOrElse(""); i += 1 }
          out ++= s"<blockquote>${render(inner.mkString("\n"), depth + 1)}</blockquote>"
        case ulRe(_, _) | olRe(_, _, _) =>
          flushPara()
          val items = mutable.ListBuffer[Item]()
          var continue = true
          while (i < lines.size && continue) {
            lines(i) match {
              case ulRe(sp, t)     => items += Item(sp.replace("\t", "    ").length, ordered = false, 1, t); i += 1
              case olRe(sp, n, t)  => items += Item(sp.replace("\t", "    ").length, ordered = true, n.toIntOption.getOrElse(1), t); i += 1
              // 項目の続きの行（字下げした文）は、前の項目に足す
              case l if l.startsWith("  ") && l.trim.nonEmpty && items.nonEmpty =>
                val last = items.last; items(items.size - 1) = last.copy(text = last.text + " " + l.trim); i += 1
              case _ => continue = false
            }
          }
          out ++= renderList(items.toList)
        case l =>
          para += l; i += 1
      }
    }
    flushPara()
    out.toString
  }
}

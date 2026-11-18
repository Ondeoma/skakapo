package skakapo.core

import munit.FunSuite
import skakapo.shared.Markdown

class MarkdownSpec extends FunSuite:

  test("表: 見出し・列の寄せ・セルの中の書式. 区切りの | は \\| で打ち消せる") {
    val h = Markdown.toHtml(
      """| 名前 | 件数 | 状態 |
        ||:-----|----:|:---:|
        || **skea** | 12 | `ok` |
        || a\|b | 3 |""".stripMargin)
    assert(h.contains("<table><thead><tr><th>名前</th><th style=\"text-align:right\">件数</th><th style=\"text-align:center\">状態</th></tr></thead>"), h)
    assert(h.contains("<td><strong>skea</strong></td><td style=\"text-align:right\">12</td><td style=\"text-align:center\"><code>ok</code></td>"), h)
    assert(h.contains("<td>a|b</td>") && h.contains("<td style=\"text-align:center\"></td>"), h)
  }

  test("見出し・段落（改行は br）・区切り線・引用・コードの塊（言語）") {
    val h = Markdown.toHtml("## まとめ\n一行目\n二行目\n\n---\n> 引用の **文**\n> 続き\n\n```scala\nval x = 1 < 2\n```")
    assertEquals(h, "<h4>まとめ</h4><p>一行目<br>二行目</p><hr><blockquote><p>引用の <strong>文</strong><br>続き</p></blockquote>" +
      "<pre><span class=\"lang\">scala</span><code>val x = 1 &lt; 2</code></pre>")
  }

  test("箇条書き: 入れ子・番号つき・チェックボックス・項目の続きの行") {
    val h = Markdown.toHtml("- a\n  - a1\n  - a2\n- b\n  続き\n\n3. c\n4. d\n\n- [x] 済み\n- [ ] まだ")
    assertEquals(h, "<ul><li>a<ul><li>a1</li><li>a2</li></ul></li><li>b 続き</li></ul>" +
      "<ol start=\"3\"><li>c</li><li>d</li></ol>" +
      "<ul><li class=\"task\"><input type=\"checkbox\" disabled checked> 済み</li><li class=\"task\"><input type=\"checkbox\" disabled> まだ</li></ul>")
  }

  test("インライン: 斜体・打ち消し線・リンク（http(s) だけ）・自動リンク. snake_case や a*b*c は斜体にしない") {
    assertEquals(Markdown.fmtInline("*強調* と ~~取り消し~~"), "<em>強調</em> と <del>取り消し</del>")
    assertEquals(Markdown.fmtInline("snake_case_name と 2*3*4"), "snake_case_name と 2*3*4")
    // 太字の中にコードがあってもよい. コードの中の ** や * は飾らない
    assertEquals(Markdown.fmtInline("**A. `main` を先に作る（おすすめ）** と `a**b**`"),
      "<strong>A. <code>main</code> を先に作る（おすすめ）</strong> と <code>a**b**</code>")
    assert(Markdown.fmtInline("[PR](https://github.com/o/r/pull/1)").contains("""<a href="https://github.com/o/r/pull/1" target="_blank" rel="noopener noreferrer">PR</a>"""))
    assert(!Markdown.fmtInline("[x](javascript:alert(1))").contains("<a "))
    assert(Markdown.fmtInline("見て https://example.com/a。").contains(""">https://example.com/a</a>。"""))
  }

  test("HTML は打ち消す（表・引用・見出しの中でも）") {
    val h = Markdown.toHtml("# <script>x</script>\n| <img src=x onerror=1> | b |\n|---|---|\n| \" ' | & |\n> <b>")
    assert(!h.contains("<script") && !h.contains("<img") && !h.contains("<b>"), h)
    assert(h.contains("&lt;script&gt;") && h.contains("&quot; &#39;") && h.contains("&amp;"), h)
  }

  test("見出しの閉じの # は落とし、C# の # は残す. 細工した長い行・深い引用でも重くならない") {
    assertEquals(Markdown.toHtml("## C#"), "<h4>C#</h4>")
    assertEquals(Markdown.toHtml("### まとめ ###"), "<h5>まとめ</h5>")
    val t0 = System.nanoTime()
    Markdown.toHtml("# a" + " " * 20000 + "x")
    Markdown.toHtml(">" * 50000 + " 深い")
    assert(System.nanoTime() - t0 < 2_000_000_000L)
  }

  test("とても長い行は書式を見ず、打ち消しただけで出す. 表は列と行に上限がある") {
    val long = "**" + "x" * 10001 + "** <b>"
    assertEquals(Markdown.toHtml(long), s"<p>${"**" + "x" * 10001 + "** &lt;b&gt;"}</p>")
    val wide = Markdown.toHtml("|" * 300 + "\n" + "|---" * 300 + "|\n" + "| a " * 300 + "|")
    assertEquals("<th>".r.findAllMatchIn(wide).size, 50)
    // 表の行・箇条書きの続きの長い行も、書式を見ない
    val inTable = Markdown.toHtml("| a |\n|---|\n| **" + "y" * 10001 + "** |")
    assert(inTable.contains("**yyy") && !inTable.contains("<strong>"), inTable.take(200))
  }

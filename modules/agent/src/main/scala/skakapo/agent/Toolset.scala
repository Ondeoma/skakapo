package skakapo.agent

import cats.effect.*
import cats.syntax.all.*
import com.anthropic.models.messages.{Tool, ToolTextEditor20250728, ToolUnion}
import io.circe.{ACursor, Json}
import skakapo.core.{JsonSchema, Prompt, Text}
import skakapo.shared.*

/** 連携しているサービス（復号したトークンと、その時点のツール）. */
/** viaAgent: 利用者は連携しておらず、エージェント専用アカウントで使う. */
final case class Linked(service: ServiceDto, token: String, tools: List[McpTool], viaAgent: Boolean = false)

/** ツールを実行したときの、会話の外への影響（画面の「作ったもの」に足すリンク）. */
final case class ToolOutcome(out: Out, links: List[LinkDto] = Nil)

/**
 * 1回の実行で Claude に渡すツール.
 *  - 各サービスの MCP のツール（<キー>__<ツール>. 利用者のトークンで呼ぶ）
 *  - リポジトリの作業（取り出す・探す・ファイルの表示と編集（Anthropic の text editor）・差分・commit と push・プルリクエスト）
 *  - record_link（作ったものを画面に残す）
 * allowWrites が false なら、参照のツールだけにする（管理者の設定）.
 */
final class Toolset(linked: List[Linked], repos: List[RepositoryDto], ws: Workspace, creds: Map[String, RepoCreds], hosts: GitHosts,
    mcp: Mcp, requester: (String, String), allowWrites: Boolean, onTitle: Option[String => IO[Unit]] = None, linkHosts: Set[String] = Set.empty,
    clouds: List[(CloudCredentialDto, String)] = Nil, cloudClients: Option[CloudClients] = None):

  private val repoNames = repos.map(_.name)
  private def repo(a: ACursor): Either[String, RepositoryDto] =
    a.get[String]("repository").toOption.flatMap(n => repos.find(_.name == n)).toRight(s"repository は次のどれかにしてください: ${repoNames.mkString(", ")}")

  // ---------------------------------------------------------------- 定義

  import JsonSchema.{str, obj as schema}
  private def repoProp: Json = JsonSchema.oneOf("リポジトリの名前", repoNames)

  private final case class Builtin(name: String, description: String, input: Json, write: Boolean)

  private val builtins: List[Builtin] =
    (if repos.isEmpty then Nil else List(
      Builtin("checkout_repository", "リポジトリを作業場所（/workspace/<名前>）に取り出す（すでにあれば最新を取り込む）。branch を指定すると、そのブランチにする（なければ既定のブランチから作る）。",
        schema(List("repository"), "repository" -> repoProp, "branch" -> str("切り替えるブランチ（任意. 作業用は skakapo/ で始める）")), write = false),
      Builtin("list_files", "ディレクトリの中のファイルを一覧する（.git・node_modules・target などは除く。2000件まで）。",
        schema(List("path"), "path" -> str("例: /workspace/skea/modules")), write = false),
      Builtin("search_code", "ファイルの中身を正規表現（Java の書き方）で探し、ファイル:行: 内容 を返す（300件まで）。",
        schema(List("pattern"), "pattern" -> str("正規表現"), "path" -> str("探す場所（既定 /workspace）"), "glob" -> str("ファイル名の絞り込み（例: *.scala）")), write = false),
      Builtin("git_diff", "取り出したリポジトリの、まだコミットしていない変更（状態と差分）を見る。", schema(List("repository"), "repository" -> repoProp), write = false),
      Builtin("commit_and_push", "変更をすべてコミットし、ブランチに push する（ブランチは skakapo/ で始める. 既定のブランチには push できない）。",
        schema(List("repository", "branch", "message"), "repository" -> repoProp, "branch" -> str("例: skakapo/SKEA-12-fix-sort"),
          "message" -> str("コミットのメッセージ")), write = true),
      Builtin("create_pull_request", "push したブランチから、プルリクエストを出す（GitHub・Bitbucket・Forgejo）。",
        schema(List("repository", "title", "body", "branch"), "repository" -> repoProp, "title" -> str("題名"), "body" -> str("本文（Markdown）"),
          "branch" -> str("push したブランチ（skakapo/ で始まる）"), "base" -> str("取り込み先（既定はリポジトリの既定のブランチ）"),
          "draft" -> JsonSchema.bool("下書きにする")), write = true),
      Builtin("comment_pull_request", "プルリクエスト（GitHub・Bitbucket・Forgejo）にコメントする。",
        schema(List("repository", "number", "body"), "repository" -> repoProp, "number" -> JsonSchema.int("番号"),
          "body" -> str("本文（Markdown）")), write = true)
    )) ++ (if clouds.isEmpty || cloudClients.isEmpty then Nil else List(
      Builtin("cloud_request", "クラウド（AWS・Google Cloud・Azure）の API を、登録した資格情報で呼ぶ。応答の状態と本文を返す。" +
        "AWS は SigV4 で署名する（service と region を指定. 例: ec2・logs・s3・sts）。GCP・Azure は資格情報から発行したトークンを付ける。" +
        "送れるのは、そのクラウドの API のホストだけ。",
        schema(List("credential", "method", "url"), "credential" -> JsonSchema.oneOf("資格情報の名前", clouds.map(_._1.name)),
          "method" -> JsonSchema.oneOf("HTTP のメソッド", List("GET", "POST", "PUT", "PATCH", "DELETE", "HEAD")),
          "url" -> str("https:// の URL（例: https://ec2.ap-northeast-1.amazonaws.com/?Action=DescribeInstances&Version=2016-11-15）"),
          "headers" -> JsonSchema.anyObj("追加のヘッダー（例: {\"content-type\": \"application/json\", \"x-amz-target\": \"Logs_20140328.DescribeLogGroups\"}）"),
          "body" -> str("本文（任意）"), "service" -> str("AWS のサービス名（AWS のときは必須）"), "region" -> str("AWS のリージョン（任意. 既定は資格情報のリージョン）")),
        write = true)
    )) ++ onTitle.map(_ => Builtin("set_title", "会話の題名を決める（依頼の内容を短く表す. 40文字まで）。題名が仮のときに、最初に1回だけ呼ぶ。",
      schema(List("title"), "title" -> str("題名")), write = false)).toList :+ Builtin("record_link", "作ったもの（タスク・コメント・プルリクエスト・ページなど）の URL を、会話の画面の「作ったもの」に残す。作ったらすぐに呼ぶ。",
      schema(List("label", "url"), "label" -> str("例: SKEA-12 の不具合のタスク"), "url" -> str("https:// の URL")), write = false)

  /** MCP のツール（書き込みを許さないときは、参照と分かっているものだけ）. 名前の順に並べる（プロンプトのキャッシュのため）. */
  private val mcpTools: List[(String, Linked, McpTool)] =
    linked.flatMap(l => l.tools.filter(t => allowWrites || t.info.readOnly.contains(true)).flatMap(t =>
      ToolNames.mcp(l.service.key, t.info.name).map(n => (n, l, t)))).sortBy(_._1)

  /** Claude に渡すツールの定義（実行のあいだ変わらないので1回だけ作る）. */
  lazy val definitions: List[ToolUnion] =
    val bs = builtins.filter(b => allowWrites || !b.write).map(b => b.name -> Tool.builder().name(b.name).description(b.description).inputSchema(Jsons.inputSchema(b.input)).build())
    val ms = mcpTools.map((n, l, t) => n -> Tool.builder().name(n).description(s"[${l.service.name}] ${Prompt.oneLine(t.info.description, 900)}")
      .inputSchema(Jsons.inputSchema(t.schema)).build())
    val editor = if repos.isEmpty then Nil else List(ToolUnion.ofTextEditor20250728(ToolTextEditor20250728.builder().build()))
    editor ++ (bs ++ ms).sortBy(_._1).map((_, t) => ToolUnion.ofTool(t))

  // ---------------------------------------------------------------- 実行

  private def needWrite(name: String): Option[Out] =
    Option.when(!allowWrites && builtins.exists(b => b.name == name && b.write))(Out.err("管理者の設定で、書き込みのツールは使えません"))

  /** 書き込みに使うリポジトリと認証（共有アカウント、なければ利用者の GitHub の連携. 短い期限のトークンは使う直前に取り直す）. */
  private def repoWithCreds(a: ACursor): IO[Either[String, (RepositoryDto, RepoCreds)]] =
    repo(a).flatMap(r => creds.get(r.name).map(r -> _)
      .toRight(s"${r.name} に push する認証がありません。管理者が共有アカウントを結び付けるか、利用者が画面の「連携」で GitHub と連携してください"))
      .flatTraverse((r, c) => c.fresh.map(_.map(r -> _)))

  /** 書き込みのツールの共通: 認証がそろわなければエラーを返す. */
  private def withCreds(a: ACursor)(f: (RepositoryDto, RepoCreds) => IO[ToolOutcome]): IO[ToolOutcome] =
    repoWithCreds(a).flatMap(_.fold(e => IO.pure(ToolOutcome(Out.err(e))), f.tupled))

  /** record_link に残せるのは、登録したサービス・リポジトリのホストの URL だけ（外のサイトへ誘導させない）. */
  private def linkAllowed(u: String): Boolean = Links.host(u).exists(h => linkHosts.isEmpty || linkHosts.contains(h))

  /** 共有アカウントのときは、だれの依頼かをコミットとプルリクエストに残す. */
  private val requestedBy: String = s"Requested-by: ${requester._1} <${requester._2}>"
  private val viaSkakapo: String = s"\n\n---\nskakapo 経由の依頼（依頼者: ${requester._1}）"
  private def authorOf(c: RepoCreds): (String, String) = c.author.getOrElse(requester)

  def run(name: String, input: Json): IO[ToolOutcome] =
    val a = input.hcursor
    needWrite(name).map(o => IO.pure(ToolOutcome(o))).getOrElse(name match
      case "str_replace_based_edit_tool" => ws.textEditor(a).map(ToolOutcome(_))
      case "checkout_repository" =>
        IO.pure(repo(a)).flatMap(_.fold(e => IO.pure(Out.err(e)), r => creds.get(r.name).traverse(_.fresh).flatMap {
          case Some(Left(e)) => IO.pure(Out.err(e))
          case c             => ws.checkout(r, c.flatMap(_.toOption).map(_.auth), a.get[String]("branch").toOption)
        })).map(ToolOutcome(_))
      case "list_files"  => ws.listFiles(a.get[String]("path").getOrElse(ws.Prefix)).map(ToolOutcome(_))
      case "search_code" =>
        a.get[String]("pattern").toOption.fold(IO.pure(Out.err("pattern を指定してください")))(p =>
          ws.search(p, a.get[String]("path").getOrElse(ws.Prefix), a.get[String]("glob").toOption)).map(ToolOutcome(_))
      case "git_diff" => IO.pure(repo(a)).flatMap(_.fold(e => IO.pure(Out.err(e)), ws.diff)).map(ToolOutcome(_))
      case "commit_and_push" =>
        withCreds(a) { (r, c) =>
          val msg = a.get[String]("message").getOrElse("")
          ws.commitAndPush(r, a.get[String]("branch").getOrElse(""), if c.shared && msg.trim.nonEmpty then s"$msg\n\n$requestedBy" else msg,
            Some(c.auth), authorOf(c)).map(ToolOutcome(_))
        }
      case "create_pull_request" =>
        val head = a.get[String]("branch").getOrElse("")
        if !head.startsWith("skakapo/") then IO.pure(ToolOutcome(Out.err("branch は skakapo/ で始まるブランチ（commit_and_push で push したもの）にしてください")))
        else withCreds(a) { (r, c) =>
            val body = a.get[String]("body").getOrElse("") + (if c.shared then viaSkakapo else "")
            hosts.createPullRequest(c, r, a.get[String]("title").getOrElse(""), body, head,
              a.get[String]("base").getOrElse(r.defaultBranch), a.get[Boolean]("draft").getOrElse(false)).map { o =>
              ToolOutcome(o, Links.find(o.text).filter(u => u.contains("/pull/") || u.contains("/pull-requests/") || u.contains("/pulls/"))
                .map(u => LinkDto(s"${r.name} のプルリクエスト", u)).toList.filterNot(_ => o.isError))
            }
          }
      case "comment_pull_request" =>
        a.get[Int]("number").toOption.fold(IO.pure(ToolOutcome(Out.err("number を指定してください"))))(n =>
          withCreds(a)((r, c) => hosts.comment(c, r, n, a.get[String]("body").getOrElse("") + (if c.shared then viaSkakapo else "")).map(ToolOutcome(_))))
      case "cloud_request" =>
        (cloudClients, a.get[String]("credential").toOption.flatMap(n => clouds.find(_._1.name == n))) match
          case (Some(cc), Some((c, secret))) =>
            cc.request(c, secret, CloudCall(a.get[String]("method").getOrElse("GET"), a.get[String]("url").getOrElse(""),
              a.get[Map[String, String]]("headers").getOrElse(Map.empty), a.get[String]("body").getOrElse(""),
              a.get[String]("service").toOption, a.get[String]("region").toOption)).map(ToolOutcome(_))
          case _ => IO.pure(ToolOutcome(Out.err(s"credential は次のどれかにしてください: ${clouds.map(_._1.name).mkString(", ")}")))
      case "set_title" =>
        (onTitle, a.get[String]("title").toOption.map(Prompt.oneLine(_, 60)).filter(_.nonEmpty)) match
          case (Some(f), Some(t)) => f(t).as(ToolOutcome(Out(s"題名を「$t」にしました")))
          case (None, _)          => IO.pure(ToolOutcome(Out.err("題名は利用者が決めたので、変えられません")))
          case _                  => IO.pure(ToolOutcome(Out.err("title を入れてください")))
      case "record_link" =>
        (a.get[String]("label").toOption.map(_.trim).filter(l => l.nonEmpty && l.length <= 200), a.get[String]("url").toOption.map(_.trim)
          .filter(u => (u.startsWith("https://") || u.startsWith("http://")) && u.length <= 2000 && linkAllowed(u))) match
          case (Some(l), Some(u)) => IO.pure(ToolOutcome(Out("画面に残しました"), List(LinkDto(l, u))))
          case _ => IO.pure(ToolOutcome(Out.err("label（200文字まで）と、連携しているサービス・リポジトリの url を指定してください")))
      case other =>
        mcpTools.find(_._1 == other) match
          case None => IO.pure(ToolOutcome(Out.err(s"ツール「$other」はありません（連携が外れたか、使えないツールです）")))
          case Some((_, l, t)) =>
            mcp.call(l.service.mcpUrl, l.token, t.info.name, input).attempt.map {
              case Right(r) => ToolOutcome(Out(if r.text.isEmpty then "（結果なし）" else r.text, r.isError))
              case Left(e: McpError) => ToolOutcome(Out.err(s"${l.service.name} の呼び出しに失敗しました: ${e.message}"))
              case Left(e) => ToolOutcome(Out.err(s"${l.service.name} の呼び出しに失敗しました（${e.getClass.getSimpleName}: ${Text.errMsg(e, 200)}）"))
            }
    )

object Links:
  def host(u: String): Option[String] = scala.util.Try(java.net.URI.create(u.trim).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)

  /** 文の中の最初の URL. */
  def find(s: String): Option[String] = "https?://[^\\s)）」]+".r.findFirstIn(s)

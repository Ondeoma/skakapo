package skakapo.core

import skakapo.shared.*

/** 初めて起動したときに入れておく設定（管理画面で変えられる）. */
object Defaults {

  /** sBirds シリーズ（URL と OAuth のクライアントは、管理者が環境に合わせて入れる. 入れるまでは無効）. */
  final case class ServiceSeed(key: String, name: String, kind: String, description: String, oauth: Boolean = true, mcp: Boolean = true)

  val services: List[ServiceSeed] = List(
    ServiceSeed("skea", "skea", ServiceKind.Sbirds, "プロジェクト管理（タスク・ボード・スプリント・ヘルプデスク）。不具合や依頼の起票先。"),
    ServiceSeed("stui", "stui", ServiceKind.Sbirds, "チャット（チャンネル・スレッド・ダイレクトメッセージ）。"),
    ServiceSeed("skiwi", "skiwi", ServiceKind.Sbirds, "Wiki（ページ・ブロック・データベース）。仕様書や手順書の置き場。"),
    ServiceSeed("skawau", "skawau", ServiceKind.Sbirds, "ファイルの保管と共有。"),
    ServiceSeed("skaka", "skaka", ServiceKind.Sbirds, "ビデオ会議（会議室・録画）。"),
    ServiceSeed("skuaka", "skuaka", ServiceKind.Sbirds, "予定表（カレンダー・会議室の予約）。"),
    ServiceSeed("skereru", "skereru", ServiceKind.Sbirds, "メール（Web メール・アドレス帳）。", oauth = false),
    ServiceSeed("skorimako", "skorimako", ServiceKind.Sbirds, "観測（ログ・メトリクス・トレース・アラート）。不具合の調査でエラーやトレースを調べる。"),
    ServiceSeed("skotare", "skotare", ServiceKind.Sbirds, "外部監視とステータスページ（障害・稼働率）。", oauth = false),
    ServiceSeed("smohua", "smohua", ServiceKind.Sbirds, "フォーム（アンケート・申し込み・回答の集計）。"),
    ServiceSeed("sruru", "sruru", ServiceKind.Sbirds, "ID 管理（ユーザー・グループ・OIDC/SAML のアプリ）。", oauth = false),
    ServiceSeed("github", "GitHub", ServiceKind.Github, "Git のリポジトリ（コードの取り出し・push・プルリクエスト）。", mcp = false)
  )

  /** sBirds シリーズの OAuth の入口（各サービスで共通）. */
  def oauthFor(seed: ServiceSeed, baseUrl: String): OAuthConfig =
    if (seed.kind == ServiceKind.Github) OAuthConfig("https://github.com/login/oauth/authorize", "https://github.com/login/oauth/access_token", "", "repo")
    else if (seed.oauth && baseUrl.nonEmpty) OAuthConfig(s"$baseUrl/oauth/authorize", s"$baseUrl/oauth/token", "", "read write")
    else OAuthConfig()

  val agent: AgentSettings = AgentSettings(baseContext =
    """- 回答・報告・コメント・プルリクエストの文章は日本語で書く。
      |- 社内のサービスの情報（利用者・タスク・ログなど）を、依頼に関係のない人や外部に送らない。
      |- 分からないことは推測で進めず、調べるか利用者に確かめる。""".stripMargin)

  val trackers: List[TrackerSettings] = List(TrackerSettings())

  private val bugInstructions =
    """1. フォームの内容で原因を調べられるか確かめる。再現の手順・期待する動きが足りなければ、利用者に質問して止まる。
      |2. {{tracker.service}} に不具合のタスクを起票する（プロジェクト: {{tracker.project}}）。題名は現象が分かるもの、本文にはフォームの内容と再現の手順を書く。起票したタスクのキーと URL を利用者に伝える。
      |3. 対象のサービスのリポジトリを checkout_repository で取り出し、関係するコードを search_code・str_replace_based_edit_tool で調べる。必要なら skorimako でエラーのログやトレースを調べる。
      |4. 原因が分かったら、最小限の修正を入れる。テストを足せるなら足す。
      |5. commit_and_push で skakapo/<タスクのキー>-<短い英語> のブランチに push し、create_pull_request でプルリクエストを出す。本文には原因・修正の内容・確かめ方・タスクのキーを書く。
      |6. 起票したタスクに、調査の結果・修正の内容・プルリクエストの URL をコメントする。
      |7. 利用者に、タスクとプルリクエストの URL、残っていること（確かめていないことなど）を報告する。
      |原因が分からない・修正が大きすぎるときは、5 を飛ばして、調べたことと修正の方針をタスクにコメントし、利用者に報告する。""".stripMargin

  private val featureInstructions =
    """1. 目的と、何ができれば完了かを確かめる。曖昧なら利用者に質問して止まる。
      |2. {{tracker.service}} に機能追加のタスクを起票する（プロジェクト: {{tracker.project}}）。キーと URL を伝える。
      |3. 対象のリポジトリを取り出して、変更の範囲を調べ、実装の方針をタスクにコメントする。
      |4. 変更が小さく方針がはっきりしていれば実装し、skakapo/<タスクのキー>-<短い英語> のブランチでプルリクエストを出して、URL をタスクにコメントする。
      |   大きい・判断が要るときは実装せず、方針と見積もりを報告して利用者の判断を待つ。""".stripMargin

  private val investigationInstructions =
    """1. 何を明らかにしたいのか（問い）と、範囲を確かめる。
      |2. 関係するサービスのツール（ログ・タスク・Wiki など）と、必要ならリポジトリのコードで調べる。
      |3. 分かったことを、根拠（どのデータ・どのコードか）と一緒に報告する。推測は推測と書く。
      |4. 利用者が頼んだときだけ、結果をタスクや Wiki に残す。""".stripMargin

  private val otherInstructions =
    """依頼の内容をよく読み、必要なら質問してから進める。サービスへの書き込みやコードの変更は、利用者の意図がはっきりしているときだけ行う。""".stripMargin

  private val service = FormField("service", "対象のサービス", FieldKind.Service, required = true)
  /** 件名は空でもよい（エージェントが内容から決める） */
  private val titleField = FormField("title", "件名", FieldKind.Text, placeholder = "空なら内容から自動で決めます",
    help = "空にすると、内容からエージェントが決めます（あとから変えられます）")

  val requestTypes: List[RequestTypeReq] = List(
    RequestTypeReq("bug", "不具合の修正", "動きがおかしい・エラーになるものを調べて直します（起票・修正・プルリクエストまで）", "🐞", List(
      service,
      titleField.copy(placeholder = "例: タスクの一覧で期限順に並ばない（空なら自動）"),
      FormField("description", "起きていること", FieldKind.Textarea, required = true),
      FormField("steps", "再現の手順", FieldKind.Textarea, placeholder = "1. …を開く\n2. …を押す"),
      FormField("expected", "期待する動き", FieldKind.Textarea),
      FormField("urgency", "急ぎ度", FieldKind.Select, options = List("低", "中", "高"))
    ), bugInstructions, 1),
    RequestTypeReq("feature", "機能の追加", "新しい機能や改善の依頼（起票・方針の検討・小さければ実装まで）", "✨", List(
      service,
      titleField,
      FormField("description", "ほしい機能", FieldKind.Textarea, required = true),
      FormField("purpose", "目的・困っていること", FieldKind.Textarea)
    ), featureInstructions, 2),
    RequestTypeReq("investigation", "調査", "原因や状況を調べて報告します（ログ・データ・コード）", "🔎", List(
      service.copy(required = false),
      titleField,
      FormField("question", "知りたいこと", FieldKind.Textarea, required = true),
      FormField("scope", "期間・範囲", FieldKind.Text)
    ), investigationInstructions, 3),
    RequestTypeReq("other", "その他の依頼", "上のどれにも当てはまらない依頼", "💬", List(
      titleField,
      FormField("description", "内容", FieldKind.Textarea, required = true)
    ), otherInstructions, 4)
  )

  /** 利用者の種類の例（最初に入れる. 管理画面で変えられる）. どれにも当てはまらない人はオペレータ（既定）. */
  val userGroups: List[UserGroupReq] = List(
    UserGroupReq("engineer", "エンジニア", "開発者・運用の担当者",
      """- ソースコード・ログ・設定に踏み込んで原因を調べる。
        |- 返事には、原因の箇所（リポジトリ・ファイル・行）、変更の内容、確かめた方法を具体的に書く。専門用語はそのまま使ってよい。
        |- 修正できるときは、ブランチ・コミット・プルリクエストまで進め、その URL を伝える。""".stripMargin, position = 1),
    UserGroupReq("operator", "オペレータ", "業務の担当者（開発者でない人）",
      """- 専門用語やコードの細部は書かず、何が起きていて、何をしたか・これからどうなるかを短く伝える。
        |- 調べて修正が必要なら、起票先にタスクを作り、担当のエンジニアに知らせる（コメント・メンション）。
        |- 返事は「〇〇でタスクを作り、エンジニアに知らせました」のように、したことと、作ったものの URL、次に何が起きるかを書く。""".stripMargin,
      isDefault = true, position = 2)
  )
}

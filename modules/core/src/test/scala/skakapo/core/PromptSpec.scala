package skakapo.core

import skakapo.shared.*

import java.util.UUID

class PromptSpec extends munit.FunSuite:

  private val svc = ServiceDto(UUID.randomUUID(), "skea", "skea", ServiceKind.Sbirds, "https://skea.example.com", "https://skea.example.com/mcp",
    "プロジェクト管理", "起票は SKEA プロジェクトへ", OAuthConfig(), hasSecret = false, manualToken = true, enabled = true, position = 1)
  private val repo = RepositoryDto(UUID.randomUUID(), "skea", "https://github.com/Ondeoma/skea", "main", "skea", "skea のコード", "sbt test で確かめる", true)

  test("変わらない前提に、管理者のコンテキスト・サービスとツール・リポジトリ・起票先が入る") {
    val k = Knowledge(AgentSettings(baseContext = "必ず日本語で答える"), List(TrackerSettings("既定", "skea", "SKEA"), TrackerSettings("外部", "skea", "EXT", "取引先のサイト", isDefault = false)),
      List(svc -> List(ToolInfo("create_task", "タスクを作る", Some(false)), ToolInfo("list_tasks", "一覧", Some(true)))), List(repo))
    val p = Prompt.stable(k)
    assert(p.contains("必ず日本語で答える"))
    assert(p.contains("- skea__create_task: タスクを作る"))
    assert(p.contains("- skea__list_tasks: 一覧（参照）"))
    assert(p.contains("起票は SKEA プロジェクトへ"))
    assert(p.contains("sbt test で確かめる"))
    assert(p.contains("- プロジェクト: SKEA") && p.contains("## 外部") && p.contains("- 対象: 取引先のサイト") && p.contains("## 既定（既定）"))
    // 同じ入力なら同じ文字列（キャッシュが効くように）
    assertEquals(Prompt.stable(k), p)
  }

  test("手順の変数を置き換え、フォームを最初の発言にする") {
    val rt = Defaults.requestTypes.head
    val dto = RequestTypeDto(UUID.randomUUID(), rt.key, rt.label, rt.description, rt.icon, rt.fields, rt.instructions, 1, true)
    val values = Map("service" -> "skea", "title" -> "並び順がおかしい", "description" -> "期限順に\n並ばない")
    val msg = Prompt.formMessage(dto, values)
    assert(msg.startsWith("【依頼の種類】不具合の修正"))
    assert(msg.contains("【件名】並び順がおかしい"))
    assert(msg.contains("【起きていること】\n期限順に\n並ばない"))
    assertEquals(Prompt.title(dto, values), ("並び順がおかしい", false))
    assertEquals(Prompt.title(dto, values - "title"), ("期限順に", true))
    val ts = List(TrackerSettings("外部", "skea", "EXT", isDefault = false), TrackerSettings("社内", "skea", "SKEA"))
    val s = Prompt.session(Who("山田", Some("y@example.com"), "Asia/Tokyo"), List("skea"), List("stui"), Some(dto), ts, values, "担当は経理システム", titleAuto = true)
    assert(s.contains("プロジェクト: SKEA"), s)
    assert(s.contains("担当は経理システム") && s.contains("set_title"), s)
    assert(s.contains("連携していないサービス: stui"))
    assertEquals(Prompt.expand("{{tracker.service}}/{{form.title}}/{{unknown}}", TrackerSettings("x", "skea", "P"), values), "skea/並び順がおかしい/{{unknown}}")
  }

  test("ツールの名前（<サービス>__<ツール>）") {
    assertEquals(ToolNames.mcp("skea", "create_task"), Some("skea__create_task"))
    assertEquals(ToolNames.mcp("skea", "a" * 80), None)
    assertEquals(ToolNames.split("skea__create_task"), Some("skea" -> "create_task"))
    assertEquals(ToolNames.split("checkout_repository"), None)
  }

  test("フォームの確認") {
    val fs = List(FormField("title", "件名", required = true), FormField("urgency", "急ぎ度", FieldKind.Select, options = List("低", "高")))
    assertEquals(FormCheck.values(fs, Map("title" -> " a ", "x" -> "y")), Right(Map("title" -> "a")))
    assert(FormCheck.values(fs, Map("urgency" -> "低")).isLeft)
    assert(FormCheck.values(fs, Map("title" -> "a", "urgency" -> "中")).isLeft)
    assert(FormCheck.fields(List(FormField("Bad", "x"))).isLeft)
    assert(FormCheck.fields(List(FormField("a", "x", FieldKind.Select))).isLeft)
    assert(FormCheck.fields(Defaults.requestTypes.flatMap(_.fields).distinctBy(_.name)).isRight)
  }

  test("クラウドの資格情報: 使える人（既定は管理者の依頼だけ）. 無効なものはだれも使えない") {
    val c = CloudCredentialDto(UUID.randomUUID(), "aws", CloudProvider.Aws, Map.empty, true, "", true)
    assert(c.usableBy(isAdmin = true) && !c.usableBy(isAdmin = false))
    assert(c.copy(access = CloudProvider.AccessAll).usableBy(isAdmin = false))
    assert(!c.copy(enabled = false, access = CloudProvider.AccessAll).usableBy(isAdmin = true))
  }

  test("利用者の種類: 付けた種類 → IdP のグループ（並び順） → 既定 の順に決まり、前提に回答の決まりが入る") {
    def g(key: String, idp: List[String] = Nil, default: Boolean = false, pos: Int = 0) =
      UserGroupDto(UUID.randomUUID(), key, key.toUpperCase, "", s"$key の決まり", idp, default, pos)
    val eng = g("engineer", List("dev"), pos = 1); val ops = g("operator", List("ops", "dev"), default = true, pos = 2)
    val all = List(ops, eng)
    assertEquals(UserGroups.resolve(Some(ops.id), List("dev"), all).map(_.key), Some("operator"))
    assertEquals(UserGroups.resolve(None, List("dev"), all).map(_.key), Some("engineer"))
    assertEquals(UserGroups.resolve(None, Nil, all).map(_.key), Some("operator"))
    assertEquals(UserGroups.resolve(None, Nil, List(eng)), None)
    val p = Prompt.session(Who("山田", None, "Asia/Tokyo"), Nil, Nil, None, Nil, Map.empty, group = Some(eng))
    assert(p.contains("# 利用者の種類: ENGINEER") && p.contains("engineer の決まり"), p)
  }

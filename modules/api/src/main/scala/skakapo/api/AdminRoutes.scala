package skakapo.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skakapo.agent.Runner
import skakapo.auth.SecretBox
import skakapo.core.Prompt
import skakapo.db.*
import skakapo.shared.*

import java.util.UUID

/** 管理画面: エージェントの設定（前提のコンテキスト・モデル）・連携するサービス・リポジトリ・依頼の種類・ノードの様子. */
final class AdminRoutes(config: ConfigStore, users: UserStore, nodes: NodeStore, runner: Runner, conns: skakapo.agent.Connections, hosts: skakapo.agent.GitHosts, clouds: skakapo.agent.CloudClients, box: Option[SecretBox], agentReady: Boolean, val sessions: SessionAuth,
    creditStore: CreditStore) extends RouteSupport:

  private def usd(v: Option[Double], what: String): Either[String, Option[Double]] =
    Either.cond(v.forall(x => x >= 0 && x <= 1000000 && !x.isNaN), v.map(x => math.round(x * 100) / 100.0), s"$what は 0〜1000000 ドルにしてください")

  private def url(v: String, what: String, required: Boolean = false): Either[String, String] =
    val t = v.trim.stripSuffix("/")
    if t.isEmpty then Either.cond(!required, "", s"$what を入れてください")
    else
      val uri = scala.util.Try(java.net.URI.create(t)).toOption
      // ユーザー情報（user:pass@）入りの URL は断る（秘密の値が、一覧やエラーの文に出ないように）
      Either.cond((t.startsWith("https://") || t.startsWith("http://")) && t.length <= 500 && !t.exists(_.isWhitespace) &&
        uri.exists(u => u.getHost != null && u.getRawUserInfo == null), t, s"$what は http(s):// で始まる URL にしてください（user:pass@ は入れない）")

  private def checkAgent(a: AgentSettings): Either[String, AgentSettings] =
    for
      m <- Either.cond(Models.all.exists(_._1 == a.model.trim), a.model.trim, "モデルは一覧から選んでください")
      _ <- Either.cond(Effort.all.contains(a.effort), (), "effort は low・medium・high・xhigh・max のどれかです")
      _ <- Either.cond(a.maxTurns >= 1 && a.maxTurns <= 300, (), "ターンの上限は 1〜300 です")
      _ <- Either.cond(a.maxTokens >= 1024 && a.maxTokens <= 128000, (), "応答の最大トークンは 1024〜128000 です")
      _ <- Check.text(a.baseContext, "前提のコンテキスト", 50000)
      b <- usd(a.monthlyBudgetUsd, "全体の月の予算")
      c <- usd(a.defaultUserCapUsd, "利用者ごとの上限の既定")
    yield a.copy(model = m, monthlyBudgetUsd = b, defaultUserCapUsd = c)

  /** 起票先: 1〜20件・名前は重ならない・既定はちょうど1つ（なければ最初のもの）. */
  private def checkTrackers(ts: List[TrackerSettings]): Either[String, List[TrackerSettings]] =
    val clean = ts.map(t => t.copy(name = t.name.trim, service = t.service.trim, project = t.project.trim))
    if clean.isEmpty || clean.size > 20 then Left("起票先は1〜20件にしてください")
    else if clean.exists(t => t.name.isEmpty || t.name.length > 50) then Left("起票先の名前を50文字以内で入れてください")
    else if clean.map(_.name).distinct.size != clean.size then Left("起票先の名前が重なっています")
    else if clean.exists(t => !Keys.service.matches(t.service)) then Left("起票先のサービスは、サービスのキー（例: skea）で入れてください")
    else if clean.exists(t => t.project.length > 100 || t.scope.length > 2000 || t.notes.length > 5000) then Left("起票先の設定が長すぎます")
    else
      val d = clean.indexWhere(_.isDefault).max(0)
      Right(clean.zipWithIndex.map((t, i) => t.copy(isDefault = i == d)))

  private def checkService(r: ServiceReq): Either[String, ServiceReq] =
    for
      _    <- Either.cond(Keys.service.matches(r.key), (), "キーは英小文字で始まる英小文字・数字（2〜20文字）にしてください")
      name <- Check.name(r.name)
      _    <- Either.cond(ServiceKind.all.contains(r.kind), (), "種類が正しくありません")
      base <- url(r.baseUrl, "URL")
      mcp  <- url(r.mcpUrl, "MCP の URL")
      az   <- url(r.oauth.authorizeUrl, "OAuth の認可の URL")
      tk   <- url(r.oauth.tokenUrl, "OAuth のトークンの URL")
      _    <- Check.text(r.description, "説明", 5000)
      _    <- Check.text(r.notes, "決まり・注意", 20000)
      _    <- Either.cond(r.oauth.clientId.length <= 200 && r.oauth.scopes.length <= 500, (), "OAuth の設定が長すぎます")
      _    <- Either.cond(r.agentAccount.trim.length <= 60 && !r.agentAccount.exists(_.isControl), (), "エージェント専用アカウントの名前は60文字までにしてください")
      _    <- Either.cond(r.agentToken.forall(t => t.trim.length <= 4000 && !t.trim.exists(_.isWhitespace)), (), "エージェント専用アカウントのキーを正しく入れてください")
      _    <- Either.cond(r.agentToken.forall(_.trim.isEmpty) || mcp.nonEmpty, (), "エージェント専用アカウントを使うには、MCP の URL を入れてください")
    yield r.copy(agentAccount = r.agentAccount.trim, name = name, baseUrl = base, mcpUrl = mcp, oauth = r.oauth.copy(authorizeUrl = az, tokenUrl = tk, clientId = r.oauth.clientId.trim))

  private def checkRepo(r: RepositoryReq): Either[String, RepositoryReq] =
    for
      _   <- Either.cond(Keys.repo.matches(r.name), (), "名前は英数字・. _ -（64文字まで）にしてください")
      u   <- url(r.url, "リポジトリの URL", required = true).filterOrElse(_.startsWith("https://"), "リポジトリの URL は https:// にしてください")
      _   <- Either.cond(r.defaultBranch.matches("^[A-Za-z0-9._/-]{1,100}$") && !r.defaultBranch.contains(".."), (), "既定のブランチの名前が正しくありません")
      _   <- Check.text(r.description, "説明", 5000)
      _   <- Check.text(r.notes, "作業の決まり", 20000)
    yield r.copy(url = u)

  private def checkAccount(r: GitAccountReq, creating: Boolean): Either[String, GitAccountReq] =
    for
      n  <- Check.name(r.name, "アカウントの名前")
      _  <- Either.cond(GitProvider.all.contains(r.provider), (), "ホストの種類が正しくありません")
      api <- url(r.apiUrl, "API の URL")
      _  <- Either.cond(r.username.length <= 100 && !r.username.exists(c => c.isWhitespace || c == ':'), (), "ユーザー名に空白や : は使えません")
      _  <- Either.cond(!GitProvider.needsUser(r.provider) || r.username.trim.nonEmpty, (), s"${GitProvider.label(r.provider)} はユーザー名を入れてください")
      _  <- Either.cond(GitAuthKind.forProvider(r.provider).contains(r.authKind), (), s"${GitProvider.label(r.provider)} では「${GitAuthKind.label(r.authKind)}」を選べません")
      what = r.authKind match
        case GitAuthKind.GithubApp      => "秘密鍵（PEM）"
        case GitAuthKind.BitbucketOAuth => "シークレット"
        case _                          => "トークン"
      _  <- Either.cond(!creating || r.token.exists(_.trim.nonEmpty), (), s"${what}を入れてください")
      _  <- Either.cond(r.token.forall { t =>
              val v = t.trim
              // 秘密鍵は複数行の PEM. ほかは空白を含まない1つの値
              if r.authKind == GitAuthKind.GithubApp then v.length <= 10000 && v.contains("PRIVATE KEY")
              else v.length <= 4000 && !v.exists(_.isWhitespace)
            }, (), s"${what}を正しく入れてください")
      _  <- Either.cond(r.authKind == GitAuthKind.Token || r.appId.trim.matches("^[A-Za-z0-9._-]{1,100}$"), (),
              if r.authKind == GitAuthKind.GithubApp then "App ID（または Client ID）を入れてください" else "コンシューマーのキーを入れてください")
      _  <- Either.cond(r.authKind != GitAuthKind.GithubApp || r.installationId.trim.matches("^[0-9]{0,20}$"), (), "Installation ID は数字で入れてください（空なら自動で探します）")
      _  <- Either.cond(r.authorName.length <= 100 && r.authorEmail.length <= 200 && !(r.authorName + r.authorEmail).exists(_.isControl), (), "作者の名前・メールが長すぎます")
    yield r.copy(name = n, apiUrl = api, username = r.username.trim, authorName = r.authorName.trim, authorEmail = r.authorEmail.trim,
      appId = if r.authKind == GitAuthKind.Token then "" else r.appId.trim,
      installationId = if r.authKind == GitAuthKind.GithubApp then r.installationId.trim else "")

  /**
   * 共有アカウントを保存する. GitHub App・OAuth コンシューマーは、保存する前に鍵・シークレットで認証できるか確かめる
   * （秘密を入れ直さないときは、保存してある秘密で確かめる）. 保存したら、覚えていたトークンを捨てる.
   */
  private def saveAccount(id: Option[UUID], r: GitAccountReq): IO[Response[IO]] =
    valid(checkAccount(r, id.isEmpty)) { x =>
      val newSecret = x.token.map(_.trim).filter(_.nonEmpty)
      val probe = GitAccountDto(id.getOrElse(ProbeId), x.name, x.provider, x.apiUrl, x.username, true, x.authorName, x.authorEmail,
        x.authKind, x.appId, x.installationId)
      val bound = id.fold(IO.pure(List.empty[RepositoryDto]))(i => config.repositories().map(_.filter(_.accountId.contains(i))))
      (id.flatTraverse(config.gitAccountWithToken), bound).flatMapN { (before, repos) =>
        // 送り先（ホストの種類・API の URL）を変えたのに秘密を入れ直さないときは断る（保存してある秘密を別のホストへ送らせない）.
        // 結び付けたリポジトリが新しい送り先のホストと違うときも断る（前のホストのリポジトリに新しい秘密を送らない）
        val moved = Option.when(newSecret.isEmpty && before.exists((b, _) => b.provider != x.provider || b.apiUrl != x.apiUrl))(
          "送り先（ホストの種類・API の URL）を変えたときは、秘密を入れ直してください")
        val mismatch = accountHost(probe).flatMap(h => repos.find(r => skakapo.agent.GitAuth.origin(r.url) != Some(h)).map(r =>
          s"結び付けたリポジトリ「${r.name}」のホストが、送り先（$h）と違います。先にリポジトリの結び付けを変えてください"))
        valid(moved.orElse(mismatch).toLeft(())) { _ =>
          probeAndSave(id, newSecret, IO.pure(before.flatMap((_, enc) => box.flatMap(_.decrypt(enc)))),
            s => if x.authKind == GitAuthKind.Token then IO.pure(Right("")) else hosts.apps.probe(probe, s, skakapo.agent.GitHosts.apiFor(x.provider, x.apiUrl, "")),
            (enc, _) => config.saveGitAccount(id, x, enc).flatTap(_.traverse_(a => hosts.apps.forget(a.id))))
        }
      }
    }

  /**
   * 秘密を確かめてから保存する（共有アカウント・クラウドの資格情報の共通）: 新しく入れた秘密、なければ保存してある秘密で確かめ、
   * 通ったら保存する（新しい秘密は暗号化する）. save は (暗号化した新しい秘密, 確かめた結果) を受け取る.
   */
  private def probeAndSave[A: _root_.io.circe.Encoder](id: Option[UUID], newSecret: Option[String], stored: IO[Option[String]],
      probe: String => IO[Either[String, String]], save: (Option[String], String) => IO[Either[String, A]]): IO[Response[IO]] =
    newSecret.fold(stored)(s => IO.pure(Some(s))).flatMap {
      case None => err(Status.BadRequest, "秘密を入れてください")
      case Some(p) => probe(p).flatMap {
          case Left(e)    => err(Status.BadRequest, e)
          case Right(who) => valid(newSecret.traverse(encrypt(_, "秘密")))(enc => saved(id)(save(enc, who)))
        }
    }

  private def checkGroup(r: UserGroupReq): Either[String, UserGroupReq] =
    for
      _ <- Either.cond(Keys.requestType.matches(r.key), (), "キーは英小文字で始まる英小文字・数字・_・-（32文字まで）にしてください")
      l <- Check.name(r.label, "表示名")
      _ <- Check.text(r.description, "説明", 1000)
      _ <- Check.text(r.instructions, "回答の決まり", 10000)
      gs = r.idpGroups.map(_.trim).filter(_.nonEmpty).distinct
      _ <- Either.cond(gs.size <= 50 && gs.forall(g => g.length <= 200 && !g.exists(_.isControl)), (), "IdP のグループは50個まで（1つ200文字まで）にしてください")
    yield r.copy(label = l, idpGroups = gs)

  /** 保存する前の確かめに使う仮の ID（まだ保存していないもの）. */
  private val ProbeId = NilId.value

  private def checkCloud(r: CloudCredentialReq, creating: Boolean): Either[String, CloudCredentialReq] =
    for
      n <- Check.name(r.name, "資格情報の名前")
      _ <- Either.cond(CloudProvider.all.contains(r.provider), (), "クラウドの種類が正しくありません")
      // 送ってよいホストは「a,b」の形にそろえる（空白を除く）
      cfg = r.config.map((k, v) => k -> (if k == CloudProvider.AllowedHosts then v.split(",").map(_.trim).filter(_.nonEmpty).mkString(",") else v.trim))
              .filter((k, v) => CloudProvider.configFields(r.provider).exists(_._1 == k) && v.nonEmpty)
      _ <- CloudProvider.configFields(r.provider).find((k, _, req) => req && !cfg.contains(k)).map((_, label, _) => s"${label}を入れてください").toLeft(())
      _ <- Either.cond(cfg.values.forall(v => v.length <= 200 && !v.exists(c => c.isWhitespace || c.isControl)), (), "設定の値に空白は使えません（200文字まで）")
      sec = r.secrets.map(_.view.mapValues(_.trim).filter((k, v) => CloudProvider.secretFields(r.provider).exists(_._1 == k) && v.nonEmpty).toMap)
      _ <- Either.cond(!creating || sec.isDefined, (), "秘密の値を入れてください")
      _ <- sec.flatMap(m => CloudProvider.secretFields(r.provider).find((k, _, req) => req && !m.contains(k)).map((_, label, _) => s"${label}を入れてください")).toLeft(())
      _ <- Either.cond(sec.forall(_.values.forall(_.length <= 20000)), (), "秘密の値が長すぎます")
      _ <- Either.cond(r.provider != CloudProvider.Gcp || sec.forall(_.get("serviceAccountJson").forall(j =>
             io.circe.parser.parse(j).toOption.exists(x => x.hcursor.get[String]("client_email").isRight && x.hcursor.get[String]("private_key").isRight))), (),
             "サービスアカウントの鍵は、ダウンロードした JSON の中身をそのまま貼り付けてください")
      _ <- Either.cond(cfg.get(CloudProvider.AllowedHosts).forall(_.split(",").forall(h => h.matches("^[a-zA-Z0-9*.-]{1,200}$") && h.count(_ == '*') <= 3)), (),
             "送ってよいホストは、ホスト名（* は1つにつき3個まで）をカンマで区切って入れてください")
      _ <- Either.cond(CloudProvider.accessAll.contains(r.access), (), "使える人が正しくありません")
      _ <- Either.cond(r.access != CloudProvider.AccessGroups || r.groups.nonEmpty, (), "使える利用者の種類を選んでください")
      _ <- Check.text(r.notes, "説明", 5000)
    yield r.copy(name = n, config = cfg, secrets = sec)

  /**
   * クラウドの資格情報を保存する. 保存する前に認証できるか確かめる（秘密を入れ直さないときは、保存してある秘密で確かめる）.
   * 確かめた結果（だれとして認証されたか）は identity で返す.
   */
  private def saveCloud(id: Option[UUID], r: CloudCredentialReq): IO[Response[IO]] =
    valid(checkCloud(r, id.isEmpty)) { x =>
      val probe = CloudCredentialDto(id.getOrElse(ProbeId), x.name, x.provider, x.config, true, x.notes, x.enabled, access = x.access, groups = x.groups)
      probeAndSave(id, x.secrets.map(_.asJson.noSpaces), id.flatTraverse(i => config.cloudSecret(i).map(_.flatMap(e => box.flatMap(_.decrypt(e))))),
        clouds.probe(probe, _),
        // AWS は確かめで分かったアカウント ID を覚える（S3 を自分のアカウントのバケットに限るため. 分からなければ保存しない）
        (enc, who) => (x.provider, clouds.accountIdOf(who)) match
          case (CloudProvider.Aws, None) => IO.pure(Left(s"AWS のアカウント ID を確かめられませんでした（$who）"))
          case (_, acct) => config.saveCloudCredential(id, x.copy(config = x.config ++ acct.map(clouds.AccountId -> _)), enc)
              .flatTap(_.traverse_(c => clouds.forget(c.id))).map(_.map(_.copy(identity = Some(who)))))
    }

  /** リポジトリの確認（結び付ける共有アカウントがあること）. */
  private def validRepo(r: RepositoryReq)(f: RepositoryReq => IO[Response[IO]]): IO[Response[IO]] =
    valid(checkRepo(r))(x => x.accountId.fold(f(x))(id => config.gitAccountWithToken(id).flatMap {
      case None => err(Status.BadRequest, "共有アカウントが見つかりません")
      // リポジトリのホストが共有アカウントのホストと違えば結び付けない（共有アカウントの秘密を別のホストへ送らせない）
      case Some((a, _)) => accountHost(a).filter(h => skakapo.agent.GitAuth.origin(x.url) != Some(h)) match
          case Some(h) => err(Status.BadRequest, s"リポジトリのホストが、共有アカウント「${a.name}」のホスト（$h）と違います")
          case None    => f(x)
    }))

  /** 共有アカウントの秘密を送ってよい git のホスト（None は決まらない: そのほか・API の URL のない Forgejo）. */
  private def accountHost(a: GitAccountDto): Option[String] =
    val api = skakapo.agent.GitAuth.origin(a.apiUrl)
    a.provider match
      case GitProvider.Github    => Some(api.filter(_ != "https://api.github.com").getOrElse("https://github.com"))
      case GitProvider.Bitbucket => Some(api.filter(_ != "https://api.bitbucket.org").getOrElse("https://bitbucket.org"))
      case GitProvider.Forgejo   => api
      case _                     => None

  /** APP_SECRET で暗号化する（なければ保存できない）. */
  private def encrypt(v: String, what: String): Either[String, String] =
    box.toRight(s"APP_SECRET が未設定のため、$what を保存できません").map(_.encrypt(v))

  /** 保存の結果: 失敗（重なりなど）は 409、新しく作ったら 201、更新なら 200. */
  private def saved[A: _root_.io.circe.Encoder](id: Option[UUID])(result: IO[Either[String, A]]): IO[Response[IO]] =
    result.flatMap(_.fold(err(Status.Conflict, _), a => if id.isEmpty then Created(a) else Ok(a)))

  private def checkType(r: RequestTypeReq): Either[String, RequestTypeReq] =
    for
      _  <- Either.cond(Keys.requestType.matches(r.key), (), "キーは英小文字で始まる英小文字・数字・_ -（32文字まで）にしてください")
      l  <- Check.name(r.label, "表示名")
      fs <- FormCheck.fields(r.fields)
      _  <- Check.text(r.description, "説明", 1000)
      _  <- Check.text(r.instructions, "手順", 30000)
      _  <- Either.cond(r.icon.length <= 8, (), "アイコンは絵文字1つにしてください")
    yield r.copy(label = l, fields = fs)

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ------------------------------------------------------------ 基本
    case req @ GET -> Root / "api" / "admin" / "settings" =>
      admin(req)(_ => (config.agent, config.trackers).flatMapN((a, t) => Ok(AdminSettingsDto(a, t, agentReady))))

    case req @ PUT -> Root / "api" / "admin" / "settings" =>
      admin(req)(u => body[AdminSettingsReq](req)(r => valid(checkAgent(r.agent)) { a =>
        valid(checkTrackers(r.trackers)) { ts =>
          config.saveSetting("agent", a, u.id) *> config.saveSetting("trackers", ts, u.id) *> Ok(AdminSettingsDto(a, ts, agentReady))
        }
      }))

    // エージェントに渡す前提（変わらない部分）を見る
    case req @ GET -> Root / "api" / "admin" / "prompt" =>
      // 管理者の依頼で見える前提（クラウドの資格情報は、管理者が使えるもの）
      admin(req)(_ => (runner.knowledge, config.cloudCredentials(enabledOnly = true)).flatMapN((k, cs) =>
        Ok(Json.obj("text" -> Prompt.stable(k.copy(clouds = cs.filter(_.usableBy(true, None)))).asJson))))

    case req @ GET -> Root / "api" / "admin" / "stats" => admin(req)(_ => nodes.stats.flatMap(Ok(_)))

    // ------------------------------------------------------------ クレジット（今月の使った額と、利用者ごとの上限）
    case req @ GET -> Root / "api" / "admin" / "credits" =>
      admin(req)(_ => (config.agent, creditStore.perUser, creditStore.totals).flatMapN { (a, rows, mt) =>
        val (month, total) = mt
        Ok(AdminCreditsDto(month, total, rows.map((u, used, runs, ov) => UserCreditDto(u.brief, used, ov.getOrElse(a.defaultUserCapUsd), ov, runs))))
      })

    case req @ PUT -> Root / "api" / "admin" / "credits" / UuidVar(uid) =>
      admin(req)(me => body[UserCapReq](req)(r =>
        if r.useDefault then creditStore.clearCap(uid) *> NoContent()
        else valid(usd(r.capUsd, "上限"))(c => creditStore.setCap(uid, c, me.id) *> NoContent())))

    // ------------------------------------------------------------ サービス
    case req @ GET -> Root / "api" / "admin" / "services" => admin(req)(_ => config.services().flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "services" =>
      admin(req)(_ => body[ServiceReq](req)(r => valid(checkService(r))(saveService(None, _))))

    case req @ PUT -> Root / "api" / "admin" / "services" / UuidVar(id) =>
      admin(req)(_ => body[ServiceReq](req)(r => valid(checkService(r))(saveService(Some(id), _))))

    case req @ DELETE -> Root / "api" / "admin" / "services" / UuidVar(id) =>
      admin(req)(_ => config.deleteService(id).ifM(conns.forgetService(id) *> NoContent(), err(Status.NotFound, "サービスが見つかりません")))

    // ------------------------------------------------------------ リポジトリ
    case req @ GET -> Root / "api" / "admin" / "repositories" => admin(req)(_ => config.repositories().flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "repositories" =>
      admin(req)(_ => body[RepositoryReq](req)(r => validRepo(r)(x => saved(None)(config.saveRepository(None, x)))))

    case req @ PUT -> Root / "api" / "admin" / "repositories" / UuidVar(id) =>
      admin(req)(_ => body[RepositoryReq](req)(r => validRepo(r)(x => saved(Some(id))(config.saveRepository(Some(id), x)))))

    // ------------------------------------------------------------ 利用者の種類
    case req @ GET -> Root / "api" / "admin" / "user-groups" => admin(req)(_ => config.userGroups.flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "user-groups" =>
      admin(req)(_ => body[UserGroupReq](req)(r => valid(checkGroup(r))(x => saved(None)(config.saveUserGroup(None, x)))))

    case req @ PUT -> Root / "api" / "admin" / "user-groups" / UuidVar(id) =>
      admin(req)(_ => body[UserGroupReq](req)(r => valid(checkGroup(r))(x => saved(Some(id))(config.saveUserGroup(Some(id), x)))))

    case req @ DELETE -> Root / "api" / "admin" / "user-groups" / UuidVar(id) =>
      admin(req)(_ => config.deleteUserGroup(id).ifM(NoContent(), err(Status.NotFound, "種類が見つかりません")))

    // 利用者の一覧（付けた種類と、実際に使う種類）
    case req @ GET -> Root / "api" / "admin" / "users" =>
      admin(req)(_ => (users.listActive, config.userGroups).flatMapN((us, gs) =>
        Ok(us.map(u => AdminUserDto(u.brief, u.isAdmin, u.idpGroups, u.groupId, UserGroups.resolve(u.groupId, u.idpGroups, gs).map(_.label))))))

    case req @ PUT -> Root / "api" / "admin" / "users" / UuidVar(id) / "group" =>
      admin(req)(_ => body[AssignGroupReq](req) { r =>
        config.userGroups.flatMap { gs =>
          if r.groupId.exists(g => !gs.exists(_.id == g)) then err(Status.BadRequest, "種類が見つかりません")
          else users.assignGroup(id, r.groupId).ifM(NoContent(), err(Status.NotFound, "利用者が見つかりません"))
        }
      })

    // ------------------------------------------------------------ クラウドの資格情報（秘密は返さない）
    case req @ GET -> Root / "api" / "admin" / "cloud-credentials" => admin(req)(_ => config.cloudCredentials().flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "cloud-credentials" => admin(req)(_ => body[CloudCredentialReq](req)(saveCloud(None, _)))

    case req @ PUT -> Root / "api" / "admin" / "cloud-credentials" / UuidVar(id) => admin(req)(_ => body[CloudCredentialReq](req)(saveCloud(Some(id), _)))

    case req @ DELETE -> Root / "api" / "admin" / "cloud-credentials" / UuidVar(id) =>
      admin(req)(_ => config.deleteCloudCredential(id).ifM(clouds.forget(id) *> NoContent(), err(Status.NotFound, "資格情報が見つかりません")))

    // ------------------------------------------------------------ Git の共有アカウント（トークンは返さない）
    case req @ GET -> Root / "api" / "admin" / "git-accounts" => admin(req)(_ => config.gitAccounts.flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "git-accounts" => admin(req)(_ => body[GitAccountReq](req)(saveAccount(None, _)))

    case req @ PUT -> Root / "api" / "admin" / "git-accounts" / UuidVar(id) => admin(req)(_ => body[GitAccountReq](req)(saveAccount(Some(id), _)))

    case req @ DELETE -> Root / "api" / "admin" / "git-accounts" / UuidVar(id) =>
      admin(req)(_ => config.deleteGitAccount(id).ifM(hosts.apps.forget(id) *> NoContent(), err(Status.NotFound, "アカウントが見つかりません")))

    case req @ DELETE -> Root / "api" / "admin" / "repositories" / UuidVar(id) =>
      admin(req)(_ => config.deleteRepository(id).ifM(NoContent(), err(Status.NotFound, "リポジトリが見つかりません")))

    // ------------------------------------------------------------ 依頼の種類
    case req @ GET -> Root / "api" / "admin" / "request-types" => admin(req)(_ => config.requestTypes().flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "request-types" =>
      admin(req)(_ => body[RequestTypeReq](req)(r => valid(checkType(r))(x => saved(None)(config.saveRequestType(None, x)))))

    case req @ PUT -> Root / "api" / "admin" / "request-types" / UuidVar(id) =>
      admin(req)(_ => body[RequestTypeReq](req)(r => valid(checkType(r))(x => saved(Some(id))(config.saveRequestType(Some(id), x)))))

    case req @ DELETE -> Root / "api" / "admin" / "request-types" / UuidVar(id) =>
      admin(req)(_ => config.deleteRequestType(id).ifM(NoContent(), err(Status.NotFound, "依頼の種類が見つかりません")))
  }

  /** None は変えない、Some("") は消す、それ以外は暗号化する（APP_SECRET がなければ保存できない）. */
  private def secretField(v: Option[String], what: String): Either[String, Option[String]] = v.map(_.trim) match
    case None              => Right(None)
    case Some(s) if s.isEmpty => Right(Some(""))
    case Some(s)           => encrypt(s, what).map(Some(_))

  /**
   * サービスを保存する. エージェント専用アカウントのキーを新しく入れたら、MCP のツールの一覧を読めるかで確かめ、
   * 読めたツールをシステムとして覚える（だれも連携していなくても、エージェントが使えるように）.
   */
  private def saveService(id: Option[UUID], r: ServiceReq): IO[Response[IO]] = id.flatTraverse(config.service).flatMap { before =>
    val newAgentToken = r.agentToken.map(_.trim).filter(_.nonEmpty)
    def host(u: String) = skakapo.agent.GitAuth.origin(u)
    // 送り先（MCP・トークンの URL）を変えたのに秘密を入れ直さないときは断る（保存してある秘密を別のホストへ送らせない）
    val moved = before.flatMap { b =>
      if b.hasAgentToken && r.agentToken.isEmpty && host(b.mcpUrl) != host(r.mcpUrl) then Some("MCP の URL を変えたときは、エージェント専用アカウントのキーを入れ直してください")
      else if b.hasSecret && r.secret.isEmpty && host(b.oauth.tokenUrl) != host(r.oauth.tokenUrl) then Some("トークンの URL を変えたときは、シークレットを入れ直してください")
      else None
    }
    val check: IO[Either[String, Unit]] = newAgentToken.fold(IO.pure(Right(()))) { t =>
      val probe = ServiceDto(id.getOrElse(ProbeId), r.key, r.name, r.kind, r.baseUrl, r.mcpUrl, "", "", r.oauth, false, false, true, 0)
      conns.probe(probe, t).map(_.bimap(e => s"エージェント専用アカウントのキーで ${r.name} の MCP に接続できませんでした: $e", _ => ()))
    }
    valid(moved.toLeft(()).flatMap(_ => (secretField(r.secret, "シークレット"), secretField(r.agentToken, "エージェント専用アカウントのキー")).tupled)) { (sec, agent) =>
      check.flatMap(valid(_) { _ =>
        // 保存したら、専用アカウントのツールの一覧を忘れる. 新しいキーなら読んで覚え直す.
        // 送り先（MCP・トークン・サービスの URL）のホストが変わったら、利用者の連携を全員分外す（利用者のトークンを新しい送り先へ送らない）
        val hostsMoved = before.exists(b => host(b.mcpUrl) != host(r.mcpUrl) || host(b.oauth.tokenUrl) != host(r.oauth.tokenUrl) || host(b.baseUrl) != host(r.baseUrl))
        saved(id)(config.saveService(id, r, sec, agent).flatMap(_.flatTraverse(sid =>
          ((if hostsMoved then conns.disconnectAll(sid) else conns.forget(conns.AgentKey, sid)) *> config.service(sid))
            .flatTap(s => (s, newAgentToken).tupled.traverse_((s, t) => conns.learnTools(s, t).attempt.void))
            .map(_.toRight("サービスが見つかりません")))))
      })
    }
  }

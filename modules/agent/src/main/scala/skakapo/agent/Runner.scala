package skakapo.agent

import cats.effect.*
import cats.syntax.all.*
import cats.effect.implicits.*
import com.anthropic.errors.{AnthropicServiceException, RateLimitException}
import com.anthropic.models.messages.*
import io.circe.Json
import io.circe.syntax.*
import skakapo.core.*
import skakapo.db.*
import sbirds.observe.{Log, Metrics, Telemetry}
import sbirds.push.{PushMessage, PushService}
import skakapo.shared.*

import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/**
 * 1回の実行（利用者の発言から、エージェントが返事をするまで）を進める.
 * Claude の応答とツールの結果は1つごとに DB の transcript に保存するので、ノードが止まっても別のノードが続きから引き継げる.
 */
final class Runner(
  sessions: SessionStore,
  users: UserStore,
  config: ConfigStore,
  connections: Connections,
  claude: Option[Llm],
  mcp: Mcp,
  hosts: GitHosts,
  push: Option[PushService],
  workBase: Path,
  credits: Credits,
  box: Option[skakapo.auth.SecretBox],
  clouds: CloudClients) {

  private val MaxResultChars = 30000

  /** 前提（サービス・ツール・リポジトリ）. ツールは覚えているもの（だれかが連携したときに読んだもの）を使う. */
  def knowledge: IO[Knowledge] =
    (config.agent, config.trackers, config.services(enabledOnly = true), config.repositories(enabledOnly = true))
      .mapN(
        (
          a,
          t,
          ss,
          rs
        ) => Knowledge(a, t, ss.map(s => s -> s.tools), rs)
      )

  /**
   * 使えるサービス（トークンと、そのときのツール. 読めなかったサービスは理由を返す）.
   * 利用者が自分で連携していればその権限で、していなければ（期限が切れていれば）エージェント専用アカウントで使う.
   */
  private def linked(
    userId: java.util.UUID,
    services: List[ServiceDto]
  ): IO[(List[Linked], Option[(ServiceDto, String)], List[String])] =
    (connections.tokens(userId), connections.agentTokens).flatMapN(
      (
        tokens,
        agents
      ) =>
        services.parTraverseN(4) { s =>
          def use(
            t: String,
            viaAgent: Boolean
          ) =
            connections.tools(if viaAgent then connections.AgentKey else userId, s, t).attempt.map {
              case Right(ts) => (Some(Linked(s, t, ts, viaAgent)), None, None)
              case Left(e) => (None, None, Some(s"${s.name}（ツールを読めません: ${Text.errMsg(e, 120)}）"))
            }
          (tokens.get(s.id), agents.get(s.id)) match {
            case (Some(Some(t)), _) if s.kind == ServiceKind.Github => IO.pure((None, Some(s -> t), None))
            case _ if s.mcpUrl.isEmpty || s.kind == ServiceKind.Github => IO.pure((None, None, None))
            case (Some(Some(t)), _) => use(t, viaAgent = false)
            case (_, Some(a)) => use(a, viaAgent = true)
            case (Some(None), None) => IO.pure((None, None, Some(s"${s.name}（連携の期限が切れました。画面の「連携」で連携し直してください）")))
            case _ => IO.pure((None, None, None))
          }
        }
    ).map(xs => (xs.flatMap(_._1), xs.flatMap(_._2).headOption, xs.flatMap(_._3)))

  private def repair(
    ms: List[MessageParam]
  ): List[MessageParam] = {
    val enc = Jsons.encodeTranscript(ms)
    val fixed = Transcript.repair(enc)
    if fixed == enc then ms else Jsons.decodeTranscript(fixed)
  }

  /**
   * リポジトリごとの書き込みの認証. 共有アカウントを結び付けたリポジトリはそのアカウント、なければ利用者の GitHub の連携
   * （利用者のトークンは、リポジトリのホストが GitHub のサービスのホストと同じときだけ使う. ほかのホストへ送らない）.
   */
  private def repoCreds(
    repos: List[RepositoryDto],
    github: Option[(ServiceDto, String)]
  ): IO[Map[String, RepoCreds]] =
    repos.traverse { r =>
      r.accountId match {
        case Some(id) =>
          config.gitAccountWithToken(id).map(_.flatMap {
            (
              a,
              enc
            ) =>
              box.flatMap(_.decrypt(enc)).map { secret =>
                val api = GitHosts.apiFor(a.provider, a.apiUrl, r.url)
                val author = Option.when(a.authorName.nonEmpty)(a.authorName ->
                  (if a.authorEmail.nonEmpty then a.authorEmail else "skakapo@localhost"))
                val base = RepoCreds(
                  a.provider,
                  api,
                  GitAuth(if a.username.nonEmpty then a.username else GitProvider.defaultUser(a.provider), secret),
                  author,
                  shared = true,
                  a.name
                )
                // GitHub App・OAuth コンシューマーは、使う直前にトークンを発行する（作者の指定がなければ App のボット）
                r.name ->
                  (if a.authKind == GitAuthKind.Token then base
                   else
                     base.copy(
                       auth = GitAuth("", ""),
                       renew = Some(hosts.apps.token(a, secret, r, api).map(_.map(t =>
                         base.copy(auth = t.auth, author = author.orElse(t.author))
                       )))
                     ))
              }
          })
        case None =>
          IO.pure(github.filter(
            (
              svc,
              _
            ) =>
              Links.host(r.url).exists(h =>
                Links.host(if svc.baseUrl.nonEmpty then svc.baseUrl else "https://github.com").contains(h)
              )
          )
            .map(
              (
                svc,
                t
              ) =>
                r.name -> RepoCreds(
                  GitProvider.Github,
                  hosts.apiBase(svc.baseUrl),
                  GitAuth("x-access-token", t),
                  None,
                  shared = false,
                  "GitHub の連携"
                )
            ))
      }
    }.map(_.flatten.toMap)

  /** 実行する. cancelled は取り消しの合図（借りを延ばすときに分かる）. 終わった状態（done・failed・cancelled）と、失敗の理由を返す. */
  def run(
    r: Run,
    cancelled: IO[Boolean],
    node: String
  ): IO[(String, Option[String])] =
    sessions.find(r.sessionId).flatMap {
      case None => IO.pure((RunStatus.Failed, Some("会話が見つかりません")))
      case Some(s) =>
        users.find(s.userId).flatMap {
          case None => IO.pure((RunStatus.Failed, Some("利用者が見つかりません")))
          case Some(u) => Telemetry.withUser(
              u.id.toString,
              u.name
            )(Telemetry.span("agent.run", "request_type" -> s.requestType)(go(r, s, u, cancelled, node)))
        }
    }

  private def go(
    r: Run,
    s: AgentSession,
    u: User,
    cancelled: IO[Boolean],
    node: String
  ): IO[(String, Option[String])] =
    claude match {
      case None =>
        val msg = "Claude API のキー（ANTHROPIC_API_KEY）が設定されていないため、エージェントを動かせません。管理者に連絡してください。"
        fail(s, msg)
      case Some(cl) =>
        for {
          k0 <- knowledge
          group <- config.userGroups.map(UserGroups.resolve(u.groupId, u.idpGroups, _))
          // クラウドの資格情報: この利用者が使えるもので、秘密を復号できたものだけ（前提にも、それだけを書く）
          cloud <-
            config.enabledCloudCredentialsWithSecret.map(_.filter(_._1.usableBy(u.isAdmin, group.map(_.id))).flatMap(
              (
                c,
                enc
              ) => box.flatMap(_.decrypt(enc)).map(c -> _)
            ))
          k = k0.copy(clouds = cloud.map(_._1))
          rt <- config.requestType(s.requestType)
          all = k.services.map(_._1)
          lk <- linked(u.id, all)
          (ls, git, broken) = lk
          ms0 <- sessions.transcript(s.id).map(Jsons.decodeTranscript)
          uctx <- config.userContext(u.id)
          _ <- sessions.event(
            s.id,
            "status",
            Json.obj(
              "text" -> (if r.attempts > 1 then s"$node が続きから引き継ぎました" else s"$node が作業を始めました").asJson,
              "node" -> node.asJson
            )
          )
          _ <- broken.traverse_(b => note(s, "note", s"連携しているサービスのうち、$b"))
          ws = Workspace.forSession(workBase, s.id)
          // 題名が仮のものなら、エージェントが set_title で決める（利用者が付けた題名は変えない）
          onTitle = Option.when(s.titleAuto)((t: String) => sessions.setTitle(s.id, t, byAgent = true).void)
          creds <- repoCreds(k.repos, git)
          linkHosts =
            (all.map(_.baseUrl) ++ k.repos.map(_.url) ++ List("https://github.com", "https://bitbucket.org")).flatMap(
              Links.host
            ).toSet
          tools = new Toolset(
            ls,
            k.repos,
            ws,
            creds,
            hosts,
            mcp,
            (u.name, u.email.getOrElse("skakapo@localhost")),
            k.agent.allowWrites,
            onTitle,
            linkHosts,
            cloud,
            Some(clouds)
          )
          connected = ls.filterNot(_.viaAgent).map(_.service.name) ++ git.map(_._1.name).toList
          viaAgent = ls.filter(_.viaAgent).map(l =>
            s"${l.service.name}（アカウント「${Option(l.service.agentAccount).filter(_.nonEmpty).getOrElse("エージェント専用")}」）"
          )
          system = List(
            Claude.cached(Prompt.stable(k)),
            Claude.text(Prompt.session(
              Who(u.name, u.email, u.timezone),
              connected,
              all.map(_.name).filterNot(n =>
                connected.contains(n) || ls.exists(l => l.viaAgent && l.service.name == n)
              ),
              rt,
              k.trackers,
              s.form,
              uctx,
              s.titleAuto,
              viaAgent,
              group
            ))
          )
          ms = repair(ms0)
          // 修復した（結果のない tool_use を補った）ときは全体を書き直す. 以後は末尾に足すだけ
          res <- (if ms eq ms0 then IO.pure(true)
                  else sessions.saveTranscript(s.id, Jsons.encodeTranscript(ms), r.id, node)).flatMap {
            case true => loop(cl, k.agent, s, r, system, tools, ms, 0, cancelled)
            case false => IO.pure((RunStatus.Cancelled, None))
          }
        } yield res
    }

  private def loop(
    cl: Llm,
    a: AgentSettings,
    s: AgentSession,
    r: Run,
    system: List[TextBlockParam],
    tools: Toolset,
    ms: List[MessageParam],
    turn: Int,
    cancelled: IO[Boolean]
  ): IO[(String, Option[String])] =
    cancelled.flatMap {
      // 「止めました」は WorkerPool が書く（借りを失ったときは何も書かない）
      case true => IO.pure((RunStatus.Cancelled, None))
      case false if turn >= a.maxTurns =>
        note(s, "note", s"1回の実行のターンの上限（${a.maxTurns}）に達したので止めました。続けるには返事をしてください。")
          .as((RunStatus.Done, None))
      case false =>
        // 送る前に、今月のクレジット（全体の予算・この利用者の上限）が残っているか確かめる
        credits.blocked(s.userId, a).flatMap {
          case Some(msg) => fail(s, msg)
          case None => turnOnce(cl, a, s, r, system, tools, ms, turn, cancelled)
        }
    }

  private def turnOnce(
    cl: Llm,
    a: AgentSettings,
    s: AgentSession,
    r: Run,
    system: List[TextBlockParam],
    tools: Toolset,
    ms: List[MessageParam],
    turn: Int,
    cancelled: IO[Boolean]
  ): IO[(String, Option[String])] = {
    val node = r.node.getOrElse("")
    // 実行をまだ持っているときだけ書く. 持っていなければ（ほかのノードに移った）、ここで抜ける
    def guarded(
      write: IO[Boolean]
    )(
      rest: => IO[(String, Option[String])]
    ) =
      write.flatMap(if _ then rest
      else Log.report(ErrorCode.RunLost, s"run ${r.id} on $node").as((RunStatus.Cancelled, None)))
    def append(
      m: MessageParam
    ) = sessions.appendTranscript(s.id, Jsons.encodeTranscript(List(m)), r.id, node)
    cl.turn(a.model, a.maxTokens, a.effort, a.fallbacks, system, tools.definitions, ms).attempt.flatMap {
      case Left(e) => apiError(s, e)
      case Right(t) =>
        val m = t.message
        val next = ms :+ m.toParam()
        // server-side fallback で別のモデルが答えたときは、そのモデルの単価で数える
        val model = Option(m.model().asString()).filter(_.nonEmpty).getOrElse(a.model)
        val cost = Models.cost(model, t.inputTokens, t.outputTokens, t.cacheReadTokens, t.cacheWriteTokens)
        guarded(append(m.toParam())) {
          for
            _ <- sessions.addUsage(
              r.id,
              s.userId,
              model,
              t.inputTokens,
              t.outputTokens,
              t.cacheReadTokens,
              t.cacheWriteTokens,
              cost
            )
            _ <- Metrics.add("skakapo.claude.cost_microusd", math.round(cost * 1000000), "model" -> model)
            _ <- Metrics.count("skakapo.claude.turns", "model" -> model)
            _ <- Metrics.add("skakapo.claude.tokens", t.inputTokens, "kind" -> "input")
            _ <- Metrics.add("skakapo.claude.tokens", t.outputTokens, "kind" -> "output")
            _ <- Metrics.add("skakapo.claude.tokens", t.cacheReadTokens, "kind" -> "cache_read")
            _ <- Metrics.add("skakapo.claude.tokens", t.cacheWriteTokens, "kind" -> "cache_write")
            _ <- m.content().asScala.toList.traverse_(b =>
              b.text().toScala.traverse_(x =>
                IO.whenA(x.text().trim.nonEmpty)(sessions.event(
                  s.id,
                  "assistant",
                  Json.obj("text" -> x.text().asJson)
                ).void)
              )
            )
            res <- t.stop match {
              case Some(StopReason.TOOL_USE) =>
                val uses = m.content().asScala.toList.flatMap(_.toolUse().toScala)
                uses.traverse { tu =>
                  val input = Jsons.fromJsonValue(tu._input())
                  for {
                    _ <- sessions.event(
                      s.id,
                      "tool_call",
                      Json.obj("id" -> tu.id().asJson, "name" -> tu.name().asJson, "input" -> input)
                    )
                    o <- cancelled.ifM(
                      IO.pure(ToolOutcome(Out.err("利用者が止めたので、実行しませんでした"))),
                      Telemetry.span("agent.tool", "tool" -> tu.name())(tools.run(tu.name(), input)).handleError(e =>
                        ToolOutcome(Out.err(s"ツールの実行に失敗しました（${e.getClass.getSimpleName}: ${Text.errMsg(e, 300)}）"))
                      )
                    )
                    _ <- Metrics.count(
                      "skakapo.tools.calls",
                      "tool" -> ToolNames.split(tu.name()).fold(tu.name())(_._1 + "__*"),
                      "error" -> o.out.isError.toString
                    )
                    _ <- sessions.event(
                      s.id,
                      "tool_result",
                      Json.obj(
                        "id" -> tu.id().asJson,
                        "name" -> tu.name().asJson,
                        "text" -> Text.clip(o.out.text, 4000).asJson,
                        "isError" -> o.out.isError.asJson
                      )
                    )
                    _ <- o.links.traverse_(sessions.addLink(s.id, _))
                  } yield ContentBlockParam.ofToolResult(
                    ToolResultBlockParam.builder().toolUseId(tu.id()).content(Text.clip(o.out.text, MaxResultChars))
                      .isError(o.out.isError).build()
                  )
                }.flatMap { results =>
                  val user = MessageParam.ofUserOfContentBlockParams(results.asJava)
                  val withResults = next :+ user
                  guarded(append(user))(loop(cl, a, s, r, system, tools, withResults, turn + 1, cancelled))
                }
              case Some(StopReason.PAUSE_TURN) => loop(cl, a, s, r, system, tools, next, turn + 1, cancelled)
              case Some(StopReason.MAX_TOKENS) =>
                // 途中で切れた tool_use には結果を補ってから、続きを頼む（tool_use の次に結果がないと API が断る）
                val more = repair(next) :+ MessageParam.ofUser("（応答が長すぎて途中で切れました。続きを、短く分けて進めてください。）")
                note(s, "note", "応答が長すぎて切れたので、続きを頼みました") *>
                  guarded(sessions.saveTranscript(
                    s.id,
                    Jsons.encodeTranscript(more),
                    r.id,
                    node
                  ))(loop(cl, a, s, r, system, tools, more, turn + 1, cancelled))
              case Some(StopReason.REFUSAL) =>
                val why = m.stopDetails().toScala.flatMap(_.explanation().toScala).getOrElse("")
                val msg = s"安全のための判断で、この依頼には応えられませんでした。${why}"
                fail(s, msg)
              case Some(StopReason.MODEL_CONTEXT_WINDOW_EXCEEDED) =>
                val msg = "会話が長くなりすぎました。新しい会話で、要点をまとめて依頼し直してください。"
                fail(s, msg)
              case _ => IO.pure((RunStatus.Done, None))
            }
          yield res
        }
    }
  }

  /** 失敗を会話に書き、終わった状態にする. */
  private def fail(
    s: AgentSession,
    msg: String
  ): IO[(String, Option[String])] =
    sessions.event(s.id, "error", Json.obj("text" -> msg.asJson)).as((RunStatus.Failed, Some(msg)))

  private def note(
    s: AgentSession,
    kind: String,
    text: String
  ): IO[Unit] = sessions.event(s.id, kind, Json.obj("text" -> text.asJson)).void

  private def apiError(
    s: AgentSession,
    e: Throwable
  ): IO[(String, Option[String])] = {
    val msg = e match {
      case _: RateLimitException => "Claude API の利用の上限に達しました。しばらく待ってから、もう一度返事をしてください。"
      case x: AnthropicServiceException => s"Claude API でエラーになりました（${x.statusCode()}）。しばらく待ってから、もう一度返事をしてください。"
      case x => s"Claude API を呼べませんでした（${x.getClass.getSimpleName}）。"
    }
    Log.report(ErrorCode.ClaudeApiFailed, e.toString, e) *> fail(s, msg)
  }

  /** 終わったことを利用者に知らせる（プッシュ通知）. */
  def notifyDone(
    s: AgentSession,
    status: String
  ): IO[Unit] =
    // 利用者が止めたときは知らせない
    if status == RunStatus.Cancelled then IO.unit
    else
      push.traverse_(p =>
        p.send(
          s.userId,
          PushMessage(
            if status == RunStatus.Failed then s"⚠️ ${s.title}" else s"✅ ${s.title}",
            if status == RunStatus.Failed then "エージェントの作業が失敗しました" else "エージェントが返事をしました",
            s"/sessions/${s.id}",
            Some(s"session-${s.id}")
          )
        ).handleError(_ => ())
      )
}

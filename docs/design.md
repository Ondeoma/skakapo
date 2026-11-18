# skakapo の設計

## 会話と実行

- **会話**（`agent_session`）: 利用者ごとに、フォームから始まる。
  - `transcript` に、Claude の Messages API に渡す会話を、Java SDK の `MessageParam` の配列の JSON で持つ。
  - `form` にフォームの入力、`links` に作ったものを持つ。
- **出来事**（`session_event`）: 画面に出すもの（利用者・エージェントの発言、ツールの呼び出しと結果、状態、エラー）。
  - 画面は `GET /api/sessions/{id}/events?after=<id>&wait=20` で新しいものを待つ（長いポーリング）。
  - サーバーは DB を1秒ごとに見るので、どのノードに繋がってもよい。
- **実行**（`run`）: 利用者の発言1回ごとに、1行入れる。
  - 1つの会話で同時に動く実行は1つだけ（部分一意の索引 `run_active_idx`。発言のときは会話の行を `for update` で押さえる）。

- **題名**: フォームの件名が空なら、文の項目の最初の行を仮の題名にし、`title_auto` を立てる。
  - `title_auto` の間だけ、エージェントに `set_title` を渡す。利用者が題名を変えたら `title_auto` を下ろす（エージェントは上書きしない）。
- **添付ファイル**（`attachment`. 種類は問わない）: 画面は先に `POST /api/attachments` で上げ、ID を会話を始める要求（`StartReq`）か返事（`MessageReq`）に入れる。
  - 本人が上げて、まだ会話に結び付いていないものだけを使う。送るときに結び付け、その発言の先頭に入れる（`transcript` に残る）。中身の渡し方は `AttachmentBlocks`。
    - 最初に添付の一覧（名前・種類・大きさ・渡し方）の文を置く。
    - 続けて、画像は画像のブロック、PDF は文書のブロック、UTF-8 として読めるものは文字の文書のブロックにする。それ以外は一覧に名前を出すだけにする。
  - `transcript` はやり取りのたびに Claude へ送り直すので、1つの会話の合計を20MBまでにする（Claude の要求の上限 32MB に収める）。
  - 画像でないものは、ダウンロード（`Content-Disposition: attachment`、`application/octet-stream`）で返す。
  - 会話に結び付かないまま1日たったものは消す。見られるのは上げた本人と、会話の持ち主だけ。
- **前提の重なり**: 管理者の前提（すべての会話で同じ. キャッシュする） → 利用者が登録した前提（`user_context`） → 依頼の種類の手順。
  - 利用者の前提が管理者の前提と食い違うときは、管理者のほうに従うよう書いておく。
- **起票先**: 設定 `trackers` に複数持つ（既定はちょうど1つ）。前提にすべて並べ、エージェントが依頼の対象に合うものを選ぶ。
  - 手順の変数 `{{tracker.service}}`・`{{tracker.project}}` は既定の起票先。

## クレジット

- 応答ごとに `Models.cost`（100万トークンあたりの単価. キャッシュの読み込みは安く、書き込み（1時間）は入力の2倍）で費用を計算し、`run.cost_usd` に足す。あわせてクレジットの台帳 `credit_usage`（利用者・実行・モデル・費用）に1行書く。
- 今月（UTC）の使った額は、台帳を `created_at` で区切って合計する（会話を消しても台帳は残るので、使った額は戻らない）。実際に答えたモデル（server-side fallback）の単価で数える。
- 上限: 人ごとの上限（`user_budget`. 行がなければ既定. `cap_usd` が null なら上限なし）と、全体の月の予算（設定 `agent`）。
- 確かめるところ: 依頼を始める・返事をするとき（402）と、各ターンを送る前（止めて、理由を出来事にする）。1ターンぶんだけ上限を超えることはある。

## thinking のブロックと前提の変更

- Claude の thinking のブロックは、作られたときの前提（システムプロンプト・ツール・それまでの発言）に結び付く。
- 会話の途中で前提が変わることがある（管理者や利用者の前提、連携したサービスのツール）。
- そのため、毎回 `thinking.block_binding.prefix_mismatch_behavior: "drop_block"`（ベータ `thinking-binding-controls-2026-08-01`）を送り、合わなくなったブロックは捨てて続ける。

## 実行の取り合い（冗長構成）

1. どのノードも 2 秒ごとに、空き（`WORKER_SLOTS` − 動かしている数）があれば実行を取る。
   - 取るのは `update run … where id = (select … for update skip locked limit 1)`。複数のノードが同時に取っても重ならない。
2. **空いているノードが先に取る**: ノードは 10 秒ごとに、`node` の表に「枠」と「動かしている数」を書く（心拍）。
   - ほかに、使っている割合がこのノードより小さく、空きのある動いているノードがあれば、取らずにそちらに任せる。
   - 一番古い待っている実行が 10 秒以上待っていれば、空きのあるノードならどこでも取る（任せた先が取れないときに止まらないように）。
3. 取った実行には借り（`lease_until`. 90 秒）を付け、30 秒ごとに延ばす。
   - 延ばすときに、利用者の「止める」（`cancel_requested`）と、ほかのノードに移ったこと（借りが切れて取られた）が分かる。
4. ノードが止まると借りが切れ、ほかのノードが同じ実行を取り直して、`transcript` の続きから進める（`attempts` は3回まで。超えたら失敗にする）。
   - 最後の応答が `tool_use` で、結果がないまま止まっていたら、「結果は分からない」という `tool_result` を足してから続ける（Claude API は `tool_use` の次に結果を求める）。

## エージェントのループ（`Runner`）

1. 前提（システムプロンプト）を作る。
   - **変わらない部分**（`Prompt.stable`）: 管理者の前提のコンテキスト、サービスとそのツール（覚えているもの）、リポジトリ、起票先、作業の決まり。
     すべての会話で同じなので、プロンプトのキャッシュ（1時間）に載せる。
   - **会話ごとの部分**（`Prompt.session`）: 利用者、連携しているサービス・していないサービス、依頼の種類の手順（変数を置き換えたもの）。
2. ツールを並べる（`Toolset`）。
   - 連携しているサービスの MCP のツール（`<キー>__<ツール>`）、リポジトリの作業、`record_link`。
   - 名前の順に並べる（キャッシュのため）。
   - 管理者が「書き込みを許す」を外していれば、参照と分かっているツールだけにする。
3. Claude にストリーミングで送る（長い応答でも HTTP がタイムアウトしないように）。
   - 応答を `transcript` に保存し、文を出来事にする。
   - 停止の理由ごとに次のように進める。
     - `tool_use`: ツールを順に実行し、結果をまとめて1つの利用者の発言として足し、保存して次へ。
     - `pause_turn`: そのまま次へ。
     - `max_tokens`: 「短く分けて続けて」と足して次へ。
     - `refusal`: 安全のための判断で断られたことを伝えて終わる。既定では server-side fallback で、別のモデルに任せる。
     - `end_turn`: 返事をしたので終わる（会話は返事待ちに戻る）。
4. ターンの上限に達したら止め、続けるには返事をしてもらう。

## 連携（`Connections`）

- **OAuth**: 認可コード + PKCE（S256）。戻り先は `${PUBLIC_URL}/api/connections/callback` の1つ。
  - 連携の途中の状態は `oauth_state` に置く（どのノードに戻ってきてもよい）。
  - 戻ってきたとき、ログインしている人が始めた本人かを確かめる。
  - sBirds シリーズはクライアントの認証を Basic で、GitHub は本文の `client_secret` で行う。
- **トークン**: `APP_SECRET` の鍵で暗号化して `connection` に置く。OAuth のクライアントのシークレットも同じ。
- **ツール**: 連携したとき（とツールの読み直しのとき）に MCP の `tools/list` を読み、`service_tool` に覚える。
  - 管理画面とエージェントの前提に出し、システムとして「何ができるか」を把握する。
  - 実行のときの一覧は、利用者ごとに 5 分覚える。

### エージェント専用アカウント（サービスごと）

- `service.agent_account`（表示名）・`service.agent_token`（そのサービスで発行したキー. `APP_SECRET` で暗号化）。画面や API にはトークンを返さない（`hasAgentToken` だけ）。
- 保存するときに、新しいキーで MCP の `tools/list` を読めるか確かめる（読めなければ 400）。読めたツールはシステムとして覚える（だれも連携していなくても前提に出せる）。
- 実行のとき（`Runner.linked`）: 利用者のトークン（期限切れなら refresh）があればそれを使う。なければ専用アカウントのキーを使い、`Linked.viaAgent` にする。
  ツールの一覧は (利用者, サービス) ごとに5分覚える（専用アカウントのぶんは1つの鍵 `Connections.AgentKey`）。送り先が変わったら全員分を忘れる。
- 前提（`Prompt.session`）に「エージェント専用アカウントで使うサービス」として書き、アカウントの権限で動くこと・書き込むときは本文に依頼者を書き添えることを伝える。
- 各サービス側（skea・stui・skiwi・skereru）は、管理画面の「エージェント」で専用アカウント（ログインできない. sub は `agent:<uuid>`、stui は `bot:agent:<uuid>`）を作り、管理者がキーを発行する。

### クラウドの資格情報（`cloud_credential`・`CloudClients`）

- `config` は秘密でない値（AWS のアクセスキー ID・リージョン、GCP のプロジェクト、Azure のテナント・クライアント・サブスクリプション）、
  `secret` は秘密の値の JSON（AWS のシークレットとセッショントークン、GCP のサービスアカウントの鍵、Azure のクライアントシークレット）を `APP_SECRET` で暗号化したもの。
  項目はクラウドの種類ごとの定義（`CloudProvider.configFields`・`secretFields`）で決め、画面もここから作る。
- 保存するときに認証できるか確かめる（AWS は STS の `GetCallerIdentity`、GCP・Azure はトークンの発行）。確かめた相手（ARN など）を応答の `identity` で返す。
- `cloud_request`（書き込みのツール扱い. 書き込みを許さない設定では使えない）:
  - 送り先は https で、そのクラウドのホストだけ（Azure はホストからトークンの scope を決め、決まらない送り先には送らない）。転送はたどらない。応答は 2MB まで。
  - AWS: SigV4（canonical request・署名鍵の HMAC の連鎖. S3 は `x-amz-content-sha256` も付ける）。service は引数、region は引数か資格情報の既定。
  - GCP: RS256 の JWT（`AppJwt.rs256` を GitHub App と共用）で `oauth2.googleapis.com/token` から cloud-platform のトークンを発行する。
  - Azure: `login.microsoftonline.com/{tenant}/oauth2/v2.0/token` の client credentials。
  - トークンは (資格情報, scope) ごとに覚え、期限の5分前に発行し直す。
- 前提（`Prompt.stable`）に、資格情報の名前・クラウド・既定のリージョンなど・説明を書く（秘密は書かない）。

### 利用者の種類（`user_group`）

- 種類ごとに回答の決まり（`instructions`）と、対応する IdP のグループ（`idp_groups`）を持つ。既定の種類は1つだけ。
- ログインのたびに IdP のグループを `app_user.idp_groups` に覚える。管理者は `app_user.group_id` でその人に種類を付けられる。
- 種類は `UserGroups.resolve`（付けた種類 → IdP のグループに対応する種類（並び順） → 既定）で決め、`Prompt.session` の先頭に
  「利用者の種類」として回答の決まりを書く（利用者が登録した前提より優先する）。

## Git の作業場所（`Workspace`）

- 会話ごとに `WORK_DIR/sessions/<会話>`。エージェントには `/workspace/<リポジトリ>/…` と見せる。
  - 作業場所の外（`..`・絶対パス）と `.git` の中は扱わない。
- 取り出せるのは、管理者が登録したリポジトリだけ（`repository`）。
- Git の HTTP の認証は、`GIT_CONFIG_COUNT`・`GIT_CONFIG_KEY_n`・`GIT_CONFIG_VALUE_n` の環境変数で `http.extraHeader`（Basic）として渡す。
  - `.git/config`・URL・プロセスの引数には残さない。
  - あわせて `core.symlinks=false`・`core.hooksPath=/dev/null`・`core.fsmonitor=false`・`protocol.ext.allow=never` を渡し、取り出したリポジトリの中身で手元のコマンドが動かないようにする。
  - git のコマンドには時間切れ（clone は 10 分、fetch・push は 3 分、そのほかは 30 秒）を付ける。出力は一時ファイルに書き、パイプが詰まって止まらないようにする。
- 認証に使う資格（`RepoCreds`）は、リポジトリごとに決める。
  - リポジトリに **共有アカウント**（`git_account`）が結び付いていれば、そのトークン（`APP_SECRET` で暗号化して保存）を復号して使う。
    コミットの作者はアカウントの名前・メールにし、コミットに `Requested-by:` の行を、プルリクエストの本文に依頼者を足す（だれの依頼かを残す）。
  - 認証の方式（`git_account.auth_kind`）: `token`（アクセストークンをそのまま使う）・`github_app`・`bitbucket_oauth`。
    - `github_app`: 秘密鍵（PKCS#1・PKCS#8 の PEM）で RS256 の JWT（iss は App ID、有効9分）に署名し、Installation ID（空なら `GET /repos/{o}/{r}/installation`）の
      `POST /app/installations/{id}/access_tokens` で、`repositories` をそのリポジトリ・`permissions` を contents・pull_requests の write に絞ったトークンを発行する。
      作者の指定がなければ App のボット（`GET /app` の slug と `GET /users/{slug}[bot]` の id から `{id}+{slug}[bot]@users.noreply.github.com`）。
    - `bitbucket_oauth`: `POST https://bitbucket.org/site/oauth2/access_token`（client credentials. Basic 認証）。git は `x-token-auth`。
    - トークンは `GitApps` が (アカウント, リポジトリ) ごとに覚え、期限の5分前に発行し直す。`RepoCreds.renew` で、ツールが使う直前に取り直す（1時間を超える実行でも切れない）。
    - 保存するときに鍵・シークレットで認証できるか確かめる（GitHub App は `GET /app`）。保存したら覚えたトークンを捨てる。
  - なければ、利用者が連携した GitHub のトークンを、連携した GitHub と同じホストのリポジトリにだけ使う。
  - プルリクエストとコメントは `GitHosts` が、ホストの種類（GitHub・Bitbucket・Forgejo）ごとの API で出す。「そのほか」は push だけ。
    Forgejo は GitHub と同じパス（`/repos/{owner}/{repo}/pulls`・`/issues/{n}/comments`）で、認証は `Authorization: token …`、下書きは題名の `WIP:`。
    API の場所は空ならリポジトリのホストの `/api/v1`（`GitHosts.apiFor`）。
- 作業場所の中のパスは、`.git`（大文字小文字を問わない）と、途中のシンボリックリンクを断る。ファイルの一覧はリンクをたどらない。
- push は `skakapo/` で始まるブランチだけ（既定のブランチには push しない）。
- 7 日使っていない作業場所は消す。ほかのノードに引き継がれた会話は、取り出し直して push 済みのブランチから続ける。

## セキュリティ

### 権限と認証

- 各サービスは、利用者が連携したトークンで呼ぶ。エージェントにできることは、その利用者にできることに限られる
  （例外: 管理者が設定したエージェント専用アカウント・共有アカウント・全員に使わせるクラウドの資格情報. 運用の前提を参照）。
- 更新の要求は `X-Skakapo` ヘッダー（画面）か API トークン（Bearer）だけ（CSRF）。
- 連携の管理（OAuth の開始・トークンの保存・連携をやめる）は、画面（Cookie のセッション）からだけ受け付ける。
- 管理画面の API は、管理者（`ADMIN_GROUPS`）の画面のセッションだけ（API トークンでは使えない）。API トークンは、30 日ログインしていない人のものを使えない。
- OIDC は `OIDC_ALLOWED_GROUPS` でログインできる人を絞るのが必須。エージェントは Claude API の費用と各サービスへの操作を使うため。
- `DEV_LOGIN` のときは、`BIND_HOST` が 127.0.0.1 でなければ起動しない（`DEV_LOGIN_ALLOW_REMOTE=true` で明示したときを除く）。

### 秘密の扱い

- 秘密（OAuth のシークレット・連携のトークン・専用アカウントのキー・共有アカウントの秘密・クラウドの資格情報）は `APP_SECRET` の鍵で暗号化して保存し、画面や API には返さない。
- 秘密を保存する前に、実際に認証できるか確かめる（覚えたトークンは使わずに発行し直す）。保存・削除したら、覚えたトークンとツールの一覧を捨てる。
- 送り先を変えたときは秘密を入れ直さないと保存できない（共有アカウントの API の URL・ホストの種類、サービスの MCP・トークンの URL）。
  リポジトリは、共有アカウントのホスト（github.com・GitHub Enterprise・bitbucket.org・Forgejo の API の URL のホスト）と同じホストのときだけ結び付けられる。
  サービスの送り先のホストが変わったら、利用者の連携を全員分外す（利用者のトークンを新しい送り先へ送らない）。
- 管理画面に登録する URL には、ユーザー情報（`user:pass@`）を入れられない。

### エージェントの作業場所と git

- ツールの結果（タスクの本文・ファイル・Web の内容）は資料として扱い、その中の指示には従わないよう、作業の決まりに書く。
- エージェントがファイルを書けるのは、取り出したリポジトリの中だけ（`/workspace` の直下や、取り出していない名前には書けない）。`.git` の中とシンボリックリンクを経由するパスは扱わない。
- git には、取り出したリポジトリの `.git` を `GIT_DIR`・`GIT_WORK_TREE` で明示する（作業場所に置かれた偽の管理ファイルを使わせない）。
  グローバルの設定・git-lfs・credential helper・フック・fsmonitor は使わず、`safe.bareRepository=explicit`・`http.followRedirects=false`。
- 認証のヘッダーは `http.<リポジトリのホスト>/.extraHeader` として、そのホストにだけ送る。push の宛先は登録した URL。
- push は `skakapo/` で始まるブランチだけ。プルリクエストも `skakapo/` で始まるブランチからだけ出す。GitHub のコメントは、プルリクエストであることを確かめてから書く。
- CI の定義（`.github/workflows/`・`bitbucket-pipelines.yml` など）を変えるコミットは断る（`git diff --cached --name-only -z --no-renames` で、大文字小文字を問わず確かめる）。
- 共有アカウントのときは、コミット（`Requested-by:`）とプルリクエスト・コメントの本文に依頼者を書き添える。
- `search_code` の正規表現は、照合の途中でも20秒の期限で止める。git のコマンドには時間切れを付ける。

### クラウド（`cloud_request`）

- 送り先は、そのクラウドの API のホストだけ。AWS は `service.amazonaws.com`・`service.region.amazonaws.com` の形だけで、
  `bucket.s3…`・API Gateway・OpenSearch のドメインなど、だれでも作れる名前が前に付くホストは通さない。
- S3 は、確かめで分かったアカウント ID で `x-amz-expected-bucket-owner` を付け、自分のアカウントのバケットに限る（アカウント ID の分からない資格情報では送らない）。
- 資格情報ごとに、送ってよいホスト（`allowedHosts`. `*` は3個まで）と、使える人（`access`. 既定は管理者の依頼だけ）を決められる。

### 実行と資源

- 1人が同時に動かせる実行は 3 つまで（超えたら 429）。要求の本文は 6MB まで。外部の応答（MCP・OAuth・Git のホスト・クラウド）にも大きさの上限を付ける。
- 会話への書き込み（発言の追記・終わりの記録）は、その実行を持っているノードだけ。毎ターンの保存は末尾への追記。
- MCP のツールは、セッション切れ（404・session を含む 400）のときだけ送り直す（時間切れ・5xx で書き込みが二重にならないように）。
- クレジットは `credit_usage` の台帳で数える（会話を消しても、使った額は戻らない）。費用は実際に答えたモデルの単価で数える。
- エージェントの返事の Markdown は、先に HTML の記号をすべて打ち消してから決まった形だけを要素にする（リンクは http(s) だけ）。
  正規表現は後戻りが爆発しない形にし、1万文字を超える行は書式を見ない。引用の入れ子は8段、表は50列・2000行まで。

### 運用の前提（skakapo の中では防げないこと）
- エージェント専用アカウント（サービス）は、利用者が連携していないときに、だれの依頼でもそのアカウントの権限で動く。専用アカウントの権限は、全員に使わせてよい範囲に絞る。
- Bitbucket の OAuth コンシューマーのトークンはリポジトリに絞れない（コンシューマーの権限がそのまま付く）。コンシューマーの権限で絞る。
- クラウドの API には、リソースのポリシーで他のアカウントのリソースへ送れるものがある（AWS の SQS・SNS・EventBridge・Lambda・Kinesis、
  Azure の他のストレージアカウント・Key Vault、Google Cloud の他のプロジェクトの Cloud Storage・Pub/Sub など）。持ち出しを防ぐには、
  AWS は IAM・SCP に `aws:ResourceAccount` の条件を付け、Azure・Google Cloud は書き込みの権限を必要な範囲に絞る（組織のポリシー・VPC Service Controls など）。
- 共有アカウントの種類が「そのほか」、または API の URL のない Forgejo のときは、結び付けるリポジトリのホストを確かめられない。
- CI の定義そのものの変更は断るが、CI が動かすコード（テスト・ビルドの設定・スクリプト）は書き換えられる。
  Git のホスト側で、`skakapo/**` のブランチの CI にシークレットを渡さない（GitHub なら environment の保護・シークレットを保護したブランチに限る、
  Bitbucket なら secured な変数を保護したブランチに限る）。
- 共有アカウントのトークンには、対象のリポジトリの Contents・Pull requests の書き込みだけを与える（Workflows・管理の権限は与えない）。


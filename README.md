# skakapo

<p align="center"><img src="frontend/public/logo.svg" width="128" height="128" alt="skakapo のロゴ"></p>

skakapo は、sBirds シリーズ（社内の Web サービス群）を横断して、利用者の依頼を進める AI エージェントのサービスです。
利用者ごと・会話ごとに、Claude API（Anthropic）でエージェントを動かします。各サービスは MCP で読み書きし、Git のリポジトリを調べて直し、プルリクエストまで出します。
ログインは OpenID Connect（OIDC）。複数台で動かし、空いているノードが作業します。ライセンス: MIT

Scala 3（http4s・doobie・PostgreSQL）、Claude API は [Anthropic の Java SDK](https://github.com/anthropics/anthropic-sdk-java)。
画面は Scala.js + [Scalactive](https://github.com/Ondeoma/scalactive)、Vite です。設計は [docs/design.md](docs/design.md) を参照してください。

- **会話はフォームから始まる**: 依頼の種類（不具合の修正・機能の追加・調査・その他）を選び、その種類のフォームに入れます。
  入力はエージェントへの最初の発言になり、種類ごとの手順がエージェントに渡ります。途中で質問されたら、会話の画面で答えます。
  - **件名は空でよい**: 空にすると、エージェントが内容から題名を決めます（`set_title`）。会話の画面で題名を押すと変えられ、変えたらエージェントはもう題名を変えません。
  - **ファイルを添付できる**: 新しい依頼のフォームにも、会話の返事にも、種類を問わず添付できます（1つ10MBまで・画像は5MBまで、1回に10個まで、1つの依頼で合計20MBまで）。選ぶ・貼り付ける・ドラッグで添付します。
    - Claude には、画像は画像、PDF は文書、文字のファイル（ログ・CSV・ソースなど）は本文（長ければ先頭20万文字）として渡します。それ以外の種類は、名前・種類・大きさだけを伝えます。
- **不具合の修正の自動化**（既定の手順）: エージェントが次の順に進めます。
  1. 起票先（skea）に起票する。
  2. 対象のサービスのリポジトリを取り出して原因を調べ、最小限の修正を入れる。
  3. `skakapo/<タスクのキー>-…` のブランチに push し、プルリクエストを出す。
  4. 調査の結果・修正の内容・プルリクエストの URL をタスクにコメントし、利用者に報告する。
- **管理画面**:
  - **エージェント**
    - 前提のコンテキスト（CLAUDE.md のように、すべての会話で最初に必ず読ませる）
    - モデル（一覧から選ぶ: Claude Opus 5.5（既定）・Fable 5.2・Sonnet 5.5・Haiku 5.5）・考える深さ（effort）・ターンの上限・応答の最大トークン
    - 書き込みを許すか
    - 起票先（複数. それぞれサービス・プロジェクト・対象・決まり. エージェントは依頼の対象に合うものを選び、迷えば既定にする）
    - エージェントに渡す前提の確認
  - **サービス**
    - sBirds シリーズと、GitHub・そのほかのサービス
    - URL・MCP の URL・OAuth のクライアント
    - エージェントへの説明と決まり
    - 覚えているツールの一覧
  - **リポジトリ**
    - sBirds シリーズの外のサービスのものも登録できる
    - どのサービスのコードか、既定のブランチ
    - 作業の決まり（ビルド・テストの方法など）
  - **依頼の種類**: フォームの項目（文字・複数行・選択肢・サービス・リポジトリ）と、エージェントへの手順
    - 手順の中では、`{{tracker.service}}`・`{{tracker.project}}`・`{{form.<項目>}}` で設定や入力を差し込める
  - **ノード**: 動いているノード・抱えている実行の数・24時間の実行とトークン
- **クレジット**（Claude API の費用. ドル）: 実行ごとに、トークン数（入力・出力・キャッシュの読み書き）とモデルの単価から計算して積み上げます。
  - 管理画面の「クレジット」で、全体の月の予算と、利用者ごとの月の上限の既定を決めます。人ごとに、既定・上限なし・金額を選べます。
  - 予算や上限に達すると、新しい依頼と返事を断り（402）、動いている作業も次のターンの前で止めます。
  - 残り（自分と全体）は、画面の右上と依頼の一覧に出ます。残りが2割を切ると色が変わります。月は UTC で区切ります。
  - Anthropic のアカウントの残高ではありません（API で読めないため）。
- **自分用の前提**（利用者ごと）: 設定の画面で、エージェントに最初に読ませる自分用の前提（担当・よく使うプロジェクト・報告の好みなど）を登録できます。管理者の前提のあとに読ませ、食い違うときは管理者のほうに従わせます。
- **連携**（利用者ごと）: 各サービスに OAuth（認可コード + PKCE）でトークンを発行してもらい、skakapo が暗号化して保存します。
  OAuth に対応していないサービスは、そのサービスで発行した API トークンを貼り付けます。
- **利用者の種類**（engineer・operator など. 管理者が決める）: 種類ごとに回答の決まりを持たせ、その人の会話の前提に入れます
  （例: エンジニアにはソースコードの詳しい説明、オペレータには「タスクを作り、エンジニアに知らせました」と短く）。
  種類は、管理者がその人に付けた種類 → IdP のグループ（OIDC の groups）に対応する種類 → 既定の種類 の順で決まります。最初に engineer（エンジニア）と operator（オペレータ. 既定）の例が入ります。
- **エージェント専用アカウント**（サービスごと. 管理者が設定）: 利用者が自分で連携していないサービスは、管理者が登録した専用アカウントのキーでエージェントが使います。
  利用者が連携していれば、そちらを優先します（利用者の権限で動く）。専用アカウントで書き込むときは、本文に依頼者の名前を書き添えさせます。
  連携したときに、そのサービスの MCP の `tools/list` を読んで覚えます。こうして、システムとしてサービスごとに「何ができるか」を把握し、前提としてエージェントに渡します。
- **エージェントのツール**
  - **各サービスの MCP のツール**（`<サービスのキー>__<ツール>`）: 利用者のトークンで呼ぶので、権限は利用者と同じ
  - **リポジトリの作業**: `checkout_repository`・`list_files`・`search_code`・`git_diff`・`commit_and_push`・`create_pull_request`・`comment_pull_request`
    - ファイルの表示と編集は Anthropic の text editor（`str_replace_based_edit_tool`）
    - 作業場所の外と `.git` の中は触らない
    - push できるのは `skakapo/` で始まるブランチだけ（既定のブランチには push しない）
    - Git の認証は、その実行のあいだだけ環境変数で渡す（`.git/config` やプロセスの引数に残さない）
    - **push・プルリクエストの名義**は、リポジトリごとに選びます。
      - **共有アカウント**: 管理画面の「リポジトリ」で、GitHub・Bitbucket などに作った skakapo 専用のユーザーとトークンを登録し、リポジトリに結び付けます。
        開発者でない利用者の依頼でも、そのアカウントで commit・push・プルリクエストを出します（利用者の Git のホストとの連携は要りません）。
        コミットの作者はアカウントに登録した名前とメールにし、コミットには `Requested-by: 依頼者 <メール>`、プルリクエストの本文には依頼者を書き添えます。
        トークンは `APP_SECRET` で暗号化して保存し、画面や API には返しません。
      - **利用者の GitHub の連携**（共有アカウントを選ばないとき）: 利用者が GitHub と連携していれば、その権限で出します。トークンを送るのは、連携した GitHub と同じホストのリポジトリだけです。
  - **`cloud_request`**（管理画面の「クラウド」に資格情報を登録したとき）: AWS・Google Cloud・Azure の API を呼ぶ。読み取りも変更も、資格情報の権限のとおり
    - AWS は SigV4 で署名する。Google Cloud はサービスアカウントの鍵から、Azure はサービスプリンシパルから、トークンを発行して付ける
    - 送れるのは、そのクラウドの API のホストだけ（`*.amazonaws.com`・`*.googleapis.com`・`management.azure.com`・`graph.microsoft.com`・`*.vault.azure.net`・`*.core.windows.net` など）。
      AWS の API Gateway（`execute-api`）と S3 の静的サイト（`s3-website`）は、だれでも作れるエンドポイントなので送らない。資格情報ごとに「送ってよいホスト」でさらに絞れる
    - 資格情報ごとに「使える人」を選ぶ（管理者の依頼だけ・選んだ利用者の種類の依頼・全員の依頼。既定は管理者の依頼だけ。管理者以外にも使わせるときは、読み取り専用などに IAM で絞る）
    - 権限はクラウド側の IAM で絞る。取り返しのつかない操作は、利用者が明示的に頼んだときだけ行い、先に内容を確かめるよう前提で伝える
  - **依頼の画面**: 会話のアーカイブ・削除（一覧からも）、この依頼の使用量（費用とトークン数の合計）。細かい進捗（ツールの呼び出しと結果・作業の状態）は既定で出さず、設定の画面で利用者ごとに表示できる
  - **`record_link`**: 作ったもの（タスク・プルリクエストなど）を、会話の画面の「作ったもの」に残す
  - **`set_title`**: 題名が仮のとき（利用者が件名を入れなかったとき）だけ、内容から題名を決める
- **冗長構成**: 同じ DB を使うノードを何台でも動かせます。
  - 実行（利用者の発言1回ごと）は DB の行列に入り、空きのあるノードが `for update skip locked` で取ります。
  - **空いているノードが先に取ります**: ほかに使っている割合の小さいノードがあれば、そちらに任せます。ただし 10 秒以上待っている実行は、空きのあるノードならどこでも取ります。
  - Claude の応答とツールの結果は、1つごとに DB に保存します。ノードが止まると、借り（lease）が切れたところで、ほかのノードが続きから引き継ぎます（3回まで）。
  - 作業場所はノードごとにあります。引き継いだノードは、リポジトリを取り出し直し、push 済みのブランチから続けます。
  - `WORKER_SLOTS=0` のノードは、画面と API だけを受け持ちます。
- **画面**: 依頼の一覧（状態・作ったもの）、新しい依頼（種類 → フォーム）、会話（発言・ツールの呼び出しと結果・作業中の表示・止める・返事）、連携、管理、ダークモード、スマートフォン
- **プロンプトのキャッシュ**: システムプロンプトのうち、すべての会話で同じ部分（管理者の前提・サービスとツール・リポジトリ・決まり）は Claude のプロンプトのキャッシュ（1時間）に載せます。
- **アイコン**: 設定で画像を登録できる（正方形に切り取って縮める）。登録しない人は IdP のアイコンか名前の1文字目
- **API・AI**: REST API（API トークン. 参照のみ・読み書き）、MCP（`list_request_types`・`start_request`・`get_session`・`send_message`・`cancel_session`・`list_sessions`・`call_api`）
- **観測**（[skorimako](https://github.com/Ondeoma/skorimako)）: ログ・メトリクス・トレースを OTLP で送ります。
  - スパン: 実行ごと（`agent.run`）、ツールごと（`agent.tool`）
  - 数: Claude のターンとトークン（入力・出力・キャッシュ）、ツールの呼び出し（成功・失敗）、実行の結果と所要時間、作った会話・連携
  - ゲージ: 待っている実行・動いている実行・24時間の失敗・空きの枠・動いているノード
  - 画面のブラウザの観測（RUM）
- **プッシュ通知**（PWA）: エージェントが返事をした・失敗したときに、受け取ると決めた端末に届きます。

## 構成

| モジュール | 内容 |
|---|---|
| `modules/shared` | バックエンドと画面（Scala.js）で共有する API の型と、入力の確かめ（circe） |
| `modules/core` | エージェントの前提（システムプロンプト）の組み立て・既定の設定（依頼の種類・サービスの一覧） |
| `modules/db` | PostgreSQL（doobie・Flyway）. 設定・サービス・リポジトリ・依頼の種類・連携・会話・出来事・実行の行列・ノード |
| `modules/auth` | OIDC（Authorization Code + PKCE）、API トークン、秘密の値の暗号化 |
| `modules/devidp` | 開発・テスト用の簡易 OIDC プロバイダ |
| `modules/observe` | 観測（otel4s-oteljava） |
| `modules/push` | プッシュ通知（Web Push・VAPID） |
| `modules/agent` | Claude API（Anthropic の Java SDK）・各サービスの MCP のクライアント・OAuth の連携・Git の作業場所・GitHub・実行のループ・実行の取り合い |
| `modules/api` | REST API・MCP |
| `modules/app` | 起動（設定・HTTP サーバー・画面の配信） |
| `modules/web` | 画面（Scala.js + Scalactive） |
| `frontend` | Vite（画面のビルド）、PWA の manifest と Service Worker |

## 開発

必要なもの: JDK 21、sbt、Node.js、Docker、git。

```bash
docker compose -f docker-compose.dev.yml up -d --wait   # PostgreSQL（:54410。テスト用の DB skakapo_test も作る）
export ANTHROPIC_API_KEY=…                               # Claude API のキー（なければ画面は動くが、エージェントは動かない）
scripts/dev-restart.sh                                   # バックエンドを :8120 で起動（DEV_LOGIN=true。名前だけでログインでき、管理者も選べる）
(cd frontend && npm install && npm run dev)              # 画面の開発サーバー http://localhost:5199（/api・/auth・/mcp は :8120 へ中継）
sbt ~web/fastLinkJS                                      # 画面の変更を再リンクし続ける（ブラウザを再読み込みすると反映）
```

初めて起動すると、依頼の種類（4つ）と、sBirds シリーズ・GitHub のサービスの一覧（無効）が入ります。管理画面の「サービス」で URL と OAuth のクライアントを入れて有効にします。

**OIDC で試す**: 開発用の IdP（:9098）を起動し、バックエンドを OIDC の設定で起動し直します。

```bash
scripts/dev-idp.sh
OIDC=dev scripts/dev-restart.sh
```

**冗長構成を試す**: `scripts/dev-restart.sh` のノードは `node-1` です。同じ DB を使うノードを足します。

```bash
scripts/dev-node.sh node-2 8121
```

テストには次のものがあります。

- 前提の組み立て・フォームの確かめの単体テスト
- 会話の保存と読み戻し、作業場所の安全、手元の bare リポジトリを使った取り出し・commit・push のテスト
- 通しのテスト: 管理者の設定 → OAuth の連携 → フォームの依頼 → ノードが実行を取り、MCP のツールを呼んで返事をするまで（Claude・MCP のサービスは偽物）

```bash
sbt test
```

**DB マイグレーション**（`modules/db/src/main/resources/db/migration`）: スキーマを変えるときは新しい番号の `V<n>__*.sql` を追加し、適用済みのファイルは書き換えないでください。

## 配布・起動

```bash
scripts/dist.sh                                    # dist/lib（jar）と dist/static（画面）
docker build -f deploy/docker/app.Dockerfile -t skakapo .
```

複数台で動かすときは、同じ DB を指すノードを並べ、前にロードバランサーを置きます（例: `docker-compose.ha.yml`）。
ノードごとに `NODE_NAME` を変えます。エージェントは重いので、`WORKER_SLOTS` はマシンの大きさ（CPU・メモリ・ディスク）に合わせます。
足りなくなったら、ノードを足すだけで分担します（`skakapo.nodes.free_slots` のゲージで分かる）。

| 環境変数 | 内容 |
|---|---|
| `DATABASE_URL` / `DATABASE_USER` / `DATABASE_PASSWORD` | PostgreSQL 14 以降（`jdbc:postgresql://…`） |
| `DATABASE_POOL_SIZE` | DB の接続の数（既定 `WORKER_SLOTS` × 2 + 8） |
| `PUBLIC_URL` | 公開 URL（`https://` なら Cookie に Secure が付く。各サービスの OAuth の戻り先は `${PUBLIC_URL}/api/connections/callback`） |
| `ANTHROPIC_API_KEY` | Claude API のキー（未設定ならエージェントは動かない） |
| `WORKER_SLOTS` | このノードで同時に動かす実行の数（既定 2、0〜64。0 は画面と API だけのノード） |
| `WORK_DIR` | リポジトリを取り出す作業場所（既定 `work`。7日使っていない会話の作業場所は消す） |
| `OIDC_ISSUER` / `OIDC_CLIENT_ID` / `OIDC_CLIENT_SECRET` | OIDC。IdP には Redirect URI `${PUBLIC_URL}/auth/callback`、ログアウト後の戻り先 `${PUBLIC_URL}/` を登録する |
| `OIDC_SCOPES` | 要求するスコープ（既定 `openid profile email`） |
| `OIDC_GROUPS_CLAIM` / `OIDC_ALLOWED_GROUPS` | 所属グループのクレーム名（既定 `groups`）と、ログインを許すグループ（カンマ区切り。OIDC を使うときは必須） |
| `OIDC_ALLOW_ANY_USER` | `true` なら `OIDC_ALLOWED_GROUPS` なしで IdP のだれでもログインできる（エージェントの費用と各サービスへの操作を使えるので、IdP を自分の組織に限るときだけ） |
| `ADMIN_GROUPS` | 管理者のグループ（カンマ区切り。ログインのたびに合わせる） |
| `SESSION_DAYS` | ログインの有効日数（既定 14） |
| `HTTP_PORT` | 待ち受けるポート（既定 8080） |
| `BIND_HOST` | 待ち受けるアドレス（既定 `0.0.0.0`。`DEV_LOGIN` のときは既定 `127.0.0.1`） |
| `NODE_NAME` | このノードの名前（既定 ホスト名） |
| `APP_SECRET` | 連携のトークン・OAuth のクライアントのシークレットを暗号化する鍵（16文字以上。全ノードで同じ値にする。連携を使うときは必須） |
| `RATE_LIMIT_AUTH` / `RATE_LIMIT_PUBLIC` | IP ごとの1分あたりの回数の上限（ログイン 既定 20・公開の API 既定 120）。0 で制限しない |
| `TRUSTED_PROXIES` | リバースプロキシの IP か CIDR（カンマ区切り）。そこから来た要求だけ `X-Forwarded-For` から利用者の IP を読む |
| `STATIC_DIR` | ビルド済みの画面（`dist/static`） |
| `OTEL_EXPORTER_OTLP_ENDPOINT` / `OTEL_EXPORTER_OTLP_HEADERS` | ログ・メトリクス・トレースの送り先（skorimako の URL と `Authorization=Bearer sko_…`（ingest のキー））。未設定なら送らない |
| `LOG_FORMAT` / `LOG_LEVEL` | 標準出力のログの形（`text`（既定）・`json`）と水準（既定 `INFO`）。ログにはトレースの ID と操作したユーザーが付く |
| `SKORIMAKO_RUM_TOKEN` / `SKORIMAKO_RUM_URL` / `SKORIMAKO_RUM_SAMPLE_RATE` | 画面のブラウザの観測（skorimako の RUM）。トークンは skorimako の管理の画面で登録したアプリのもの |
| `VAPID_SUBJECT` | プッシュ通知（PWA）の連絡先（`mailto:…` か `https://…`。省略すると `PUBLIC_URL`）。VAPID の鍵は初回の起動で作って DB に保存する |
| `DEV_LOGIN` | 開発用のログインを有効にする（OIDC を設定せず、`PUBLIC_URL` が localhost の場合のみ） |
| `DEV_LOGIN_ALLOW_REMOTE` | `true` なら、開発用のログインのまま `BIND_HOST` を 127.0.0.1 以外にできる（既定では起動を断る。ロードバランサーの後ろの例でだけ使う） |

## 兄弟のサービスとの連携

各サービスの `OAUTH_CLIENTS` に skakapo を登録します。

```json
{"id":"skakapo","secret":"…（16文字以上）","redirectUri":"https://skakapo.example.com/api/connections/callback","name":"skakapo","scopes":["read","write"]}
```

登録したら、skakapo の管理画面の「サービス」に、そのサービスの URL・クライアントの ID・シークレットを入れます。
種類が sBirds シリーズのとき、「以下の URL を推論して入力」を押すと、`/mcp`・`/oauth/authorize`・`/oauth/token` が入ります。

- **skea**: 不具合の起票とコメントに使います。`scopes` に `write` を入れます（入れないと読み取りだけになり、起票できません）。
- **skorimako**: 不具合の調査で、エラーのログやトレースを調べます。
- **OAuth に対応していないサービス**（skereru・skotare・sruru など）: 利用者が、そのサービスの設定で API トークンを発行して貼り付けます。
- **GitHub**: GitHub の OAuth App（Callback URL は `${PUBLIC_URL}/api/connections/callback`）を作り、クライアントの ID とシークレットを入れます。スコープは `repo` です。
- **エージェント専用アカウント**（skea・stui・skiwi・skereru）: 各サービスの管理画面の「エージェント」で専用アカウントを作り、キーを発行します（スコープは read・write など. 管理の権限は付けられません）。
  skakapo の管理画面の「サービス」→ そのサービス →「エージェント専用アカウント」にキーを貼り付けて保存します。保存するときに MCP でツールの一覧を読み、つながることを確かめます。
  専用アカウントには、そのサービスで普通に権限を与えます（skea はプロジェクトのメンバー、skiwi はワークスペース・ページの共有、stui はチャンネルへの参加）。
- **Git の共有アカウント**（開発者でない利用者の依頼でも push・プルリクエストを出すとき）: GitHub・Bitbucket・Forgejo などに skakapo 専用のユーザーを作り、対象のリポジトリへの書き込みの権限を与えます。
  - GitHub: fine-grained personal access token（Contents・Pull requests の Read and write）。ユーザー名は空でよい。
  - Bitbucket: リポジトリのアクセストークン（ユーザー名は空。`x-token-auth` になる）か、app password（ユーザー名を入れる）。
  - **GitHub App**（個人のトークンを預けない方式. おすすめ）: GitHub App を作り（権限: Contents と Pull requests の Read and write）、
    対象のリポジトリ（または組織）にインストールします。共有アカウントの「認証の方式」で GitHub App を選び、App ID（または Client ID）と秘密鍵（.pem）を入れます。
    Installation ID は空ならリポジトリから自動で探します。skakapo は秘密鍵で署名した JWT から、そのリポジトリと上の権限だけに絞った1時間のトークンを都度発行します。
    コミットの作者を空にすると、App のボット（`<slug>[bot]`）になります。
  - **Bitbucket の OAuth コンシューマー**: ワークスペースの「OAuth consumers」で private consumer（権限: Repositories と Pull requests の Write）を作り、
    「認証の方式」で OAuth コンシューマーを選んで、キーとシークレットを入れます。client credentials で2時間のトークンを都度発行します。
  - Forgejo: アクセストークン（repository の読み書き）。ユーザー名は必須（そのアカウントのユーザー名）。API の URL は空ならリポジトリのホストの `/api/v1`。
    下書きのプルリクエストは、題名の先頭を `WIP:` にします。
  - 管理画面の「リポジトリ」の「Git の共有アカウント」に登録し、各リポジトリの「push・プルリクエストの名義」で選びます。
  - トークンに Workflows や管理の権限は与えないでください。また、`skakapo/**` のブランチの CI にはシークレットを渡さない設定にしてください（設計書の「運用の前提」）。
- **skakapo の観測**: 上の `OTEL_*`・`SKORIMAKO_RUM_*` を設定します。

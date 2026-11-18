-- skakapo のスキーマ（適用済みのファイルは書き換えず、変更は新しい番号の V<n>__*.sql を追加する）

create table app_user (
  id             uuid primary key,
  sub            text not null unique,           -- OIDC の sub（開発用ログインは dev:名前）
  name           text not null,
  email          text,
  avatar_url     text,                           -- IdP の画像
  avatar_version int,                            -- 保存したアイコンの版（null なら IdP の画像）
  timezone       text not null default 'Asia/Tokyo',
  disabled       boolean not null default false,
  is_admin       boolean not null default false,   -- 管理者（OIDC の ADMIN_GROUPS に入っている人. ログインのたびに合わせる）
  show_details   boolean not null default false,   -- 依頼の画面で、細かい進捗（ツールの呼び出しと結果・作業の状態）を表示するか
  idp_groups     text[] not null default '{}',     -- ログインのたびに IdP のグループを覚える（利用者の種類を決める）
  group_id       uuid,                             -- 管理者が付けた利用者の種類（なければ IdP のグループ・既定で決める）
  created_at     timestamptz not null default now(),
  last_login_at  timestamptz
);
create index app_user_email_idx on app_user (lower(email));

-- 利用者の種類（engineer・operator など. 管理者が決める）. 種類ごとの回答の決まり（instructions）を、その人の会話の前提に入れる.
-- 種類の決め方: 管理者がその人に付けた種類（app_user.group_id）→ IdP のグループに対応する種類（idp_groups. position の順）→ 既定の種類
create table user_group (
  id           uuid primary key,
  key          text not null unique check (key ~ '^[a-z][a-z0-9_-]{0,31}$'),
  label        text not null,
  description  text not null default '',
  instructions text not null default '',          -- エージェントへの、この種類の利用者への回答の決まり
  idp_groups   text[] not null default '{}',      -- 対応する IdP のグループ（OIDC の groups）
  is_default   boolean not null default false,    -- どれにも当てはまらない人の種類（1つだけ）
  position     int not null default 0,
  created_at   timestamptz not null default now(),
  updated_at   timestamptz not null default now()
);
create unique index user_group_default_idx on user_group (is_default) where is_default;
alter table app_user add constraint app_user_group_id_fkey foreign key (group_id) references user_group(id) on delete set null;

-- 利用者が自分で登録する、エージェントへの前提（管理者の前提のあとに読ませる）
create table user_context (
  user_id    uuid primary key references app_user(id) on delete cascade,
  text       text not null default '',
  updated_at timestamptz not null default now()
);

-- ユーザーが保存したアイコン（画面で 256×256 にした画像）. あれば IdP の avatar_url より優先する
create table user_avatar (
  user_id      uuid primary key references app_user(id) on delete cascade,
  content_type text not null check (content_type in ('image/png', 'image/jpeg', 'image/gif', 'image/webp')),
  data         bytea not null,
  updated_at   timestamptz not null default now()
);

create table session (
  token_hash bytea primary key,                  -- Cookie の値の SHA-256（値そのものは保存しない）
  user_id    uuid not null references app_user(id) on delete cascade,
  id_token   text,                               -- IdP からログアウトするときの id_token_hint
  created_at timestamptz not null default now(),
  expires_at timestamptz not null
);
create index session_expires_idx on session (expires_at);

-- OIDC のログイン途中の状態（複数ノードのどこに戻ってきてもよいよう DB に置く）
create table login_state (
  state      text primary key,
  verifier   text not null,
  nonce      text not null,
  return_to  text not null default '/',
  created_at timestamptz not null default now()
);

-- API トークン（REST API と MCP を Bearer で呼ぶため）. 値は SHA-256 だけを保存する
create table api_token (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  name         text not null,
  prefix       text not null,                    -- 画面に出す先頭部分（skakapo_xxxxxx）
  token_hash   bytea not null unique,
  scope        text not null check (scope in ('read', 'write')),
  created_at   timestamptz not null default now(),
  expires_at   timestamptz,
  last_used_at timestamptz,
  revoked_at   timestamptz
);
create index api_token_user_idx on api_token (user_id);

-- プッシュ通知（PWA）. VAPID の鍵（全ノードで同じものを使う）と、ブラウザ・端末ごとの購読
create table push_config (
  id            int primary key check (id = 1),
  vapid_public  text not null,
  vapid_private text not null,
  created_at    timestamptz not null default now()
);

create table push_subscription (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  endpoint     text not null unique,
  p256dh       text not null,
  auth         text not null,
  user_agent   text,
  created_at   timestamptz not null default now(),
  last_used_at timestamptz
);
create index push_subscription_user_idx on push_subscription (user_id);

-- ---------------------------------------------------------------- 管理画面の設定

-- 全体の設定（key ごとの JSON. agent: モデル・前提のコンテキストなど / tracker: 不具合の起票先）
create table setting (
  key        text primary key,
  value      jsonb not null,
  updated_by uuid references app_user(id) on delete set null,
  updated_at timestamptz not null default now()
);

-- 連携するサービス（sBirds シリーズ・GitHub・そのほか）. key はツールの名前の頭に付ける（skea__create_task など）.
-- OAuth のクライアントのシークレットと、エージェント専用アカウントのキーは APP_SECRET の鍵で暗号化して入れる.
-- 利用者が自分で連携していないサービスは、エージェント専用アカウント（そのサービスで発行したキー）でエージェントが使う
create table service (
  id           uuid primary key,
  key          text not null unique check (key ~ '^[a-z][a-z0-9]{1,19}$'),
  name         text not null,
  kind         text not null check (kind in ('sbirds', 'github', 'other')),
  base_url     text not null default '',
  mcp_url      text not null default '',               -- 空なら MCP を使わない（GitHub など）
  description  text not null default '',               -- 何をするサービスか（エージェントに伝える）
  notes        text not null default '',               -- 使い方の決まり・注意（エージェントに伝える）
  oauth        jsonb not null default '{}',            -- {authorizeUrl, tokenUrl, clientId, scopes}
  oauth_secret text not null default '',
  manual_token boolean not null default true,          -- 利用者が API トークンを貼り付けて連携してもよい
  agent_account text not null default '',              -- エージェント専用アカウントの名前（表示用）
  agent_token  text not null default '',               -- エージェント専用アカウントのキー（暗号化した値）
  enabled      boolean not null default true,
  position     int not null default 0,
  created_at   timestamptz not null default now(),
  updated_at   timestamptz not null default now()
);

-- サービスの MCP のツール（だれかが連携したときに tools/list で読んで覚える. システムとして「何ができるか」を把握するため）
create table service_tool (
  service_id   uuid not null references service(id) on delete cascade,
  name         text not null,
  description  text not null default '',
  input_schema jsonb not null default '{}',
  read_only    boolean,
  updated_at   timestamptz not null default now(),
  primary key (service_id, name)
);

-- Git の共有アカウント（管理者が登録する）. 開発者でない利用者の依頼でも、このアカウントで commit・push・プルリクエストを行う.
-- auth_kind: token（アクセストークン）・github_app（GitHub App のインストールのトークンを都度発行）・bitbucket_oauth（OAuth コンシューマーの client credentials）.
-- token の列には、方式ごとの秘密（トークン・秘密鍵の PEM・シークレット）を APP_SECRET の鍵で暗号化して入れる
create table git_account (
  id              uuid primary key,
  name            text not null unique,
  provider        text not null check (provider in ('github', 'bitbucket', 'forgejo', 'other')),
  api_url         text not null default '',          -- 空なら既定（GitHub・Bitbucket の API. Forgejo はリポジトリのホストの /api/v1）
  username        text not null default '',          -- git の HTTP 認証のユーザー名（空なら x-access-token / x-token-auth）
  token           text not null,
  author_name     text not null default '',          -- コミットの作者（空なら依頼者. GitHub App は App のボット）
  author_email    text not null default '',
  auth_kind       text not null default 'token' check (auth_kind in ('token', 'github_app', 'bitbucket_oauth')),
  app_id          text not null default '',          -- GitHub App の App ID（または Client ID）・OAuth コンシューマーのキー
  installation_id text not null default '',          -- GitHub App の Installation ID（空ならリポジトリから探す）
  created_at      timestamptz not null default now(),
  updated_at      timestamptz not null default now()
);

-- Git のリポジトリ（sBirds シリーズの外のサービスも含めて、どのサービスのコードか・作業の決まり）.
-- 共有アカウントを結び付けると、push・プルリクエストは常にそのアカウントで行う（なければ利用者の GitHub の連携）
create table repository (
  id             uuid primary key,
  name           text not null unique check (name ~ '^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$'),
  url            text not null,                       -- https://github.com/owner/repo(.git)
  default_branch text not null default 'main',
  service_key    text not null default '',            -- 関係するサービス（service.key・sBirds の外なら自由な名前）
  description    text not null default '',
  notes          text not null default '',            -- ビルド・テストの方法、ブランチの決まりなど
  enabled        boolean not null default true,
  account_id     uuid references git_account(id) on delete set null,
  created_at     timestamptz not null default now(),
  updated_at     timestamptz not null default now()
);

-- クラウド（AWS・Google Cloud・Azure）の資格情報. エージェントが cloud_request でクラウドの API を呼ぶときに使う.
-- config は秘密でない値（アクセスキー ID・リージョン・プロジェクト・テナント・送ってよいホストなど）. secret は秘密の値の JSON を APP_SECRET で暗号化したもの
create table cloud_credential (
  id         uuid primary key,
  name       text not null unique,
  provider   text not null check (provider in ('aws', 'gcp', 'azure')),
  config     jsonb not null default '{}',
  secret     text not null,
  notes      text not null default '',     -- エージェントに伝える説明（どの環境か・触ってよい範囲など）
  enabled    boolean not null default true,
  access     text not null default 'admins' check (access in ('admins', 'all')),   -- 使える人（管理者の依頼だけ・全員の依頼）
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- 依頼の種類（会話の最初のフォーム）. fields は入力の項目（shared の FormField の配列）、instructions はエージェントへの手順
create table request_type (
  id           uuid primary key,
  key          text not null unique check (key ~ '^[a-z][a-z0-9_-]{0,31}$'),
  label        text not null,
  description  text not null default '',
  icon         text not null default '',
  fields       jsonb not null default '[]',
  instructions text not null default '',
  position     int not null default 0,
  enabled      boolean not null default true,
  created_at   timestamptz not null default now(),
  updated_at   timestamptz not null default now()
);

-- ---------------------------------------------------------------- 利用者ごとの連携（各サービスが発行したトークン. 暗号化して保存）

create table connection (
  user_id      uuid not null references app_user(id) on delete cascade,
  service_id   uuid not null references service(id) on delete cascade,
  token        text not null,                         -- 暗号化したアクセストークン
  refresh      text not null default '',              -- 暗号化したリフレッシュトークン（あれば）
  scope        text not null default '',
  via          text not null check (via in ('oauth', 'manual')),
  expires_at   timestamptz,
  connected_at timestamptz not null default now(),
  primary key (user_id, service_id)
);

-- OAuth の連携の途中の状態（どのノードに戻ってきてもよいよう DB に置く）
create table oauth_state (
  state      text primary key,
  user_id    uuid not null references app_user(id) on delete cascade,
  service_id uuid not null references service(id) on delete cascade,
  verifier   text not null,
  return_to  text not null default '/connections',
  created_at timestamptz not null default now()
);

-- ---------------------------------------------------------------- 会話（ユーザー＋セッションごとのエージェント）

-- transcript は Claude の Messages API に渡す会話（MessageParam の配列）. ターンごとに保存するので、別のノードが続きから引き継げる
create table agent_session (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  title        text not null,
  title_auto   boolean not null default false,          -- 題名を利用者が入れなかった（エージェントが内容から決める. 利用者が変えたら止める）
  request_type text not null,
  form         jsonb not null default '{}',             -- 最初のフォームの入力（{項目: 値}）
  status       text not null default 'idle' check (status in ('idle', 'queued', 'running', 'failed')),
  transcript   jsonb not null default '[]',
  links        jsonb not null default '[]',             -- 作ったもの（skea のタスク・プルリクエストなど. [{label, url}]）
  archived     boolean not null default false,
  created_at   timestamptz not null default now(),
  updated_at   timestamptz not null default now()
);
create index agent_session_user_idx on agent_session (user_id, updated_at desc);

-- 依頼に添付した画像（Claude に渡す. 会話に結び付くまでは、上げた本人だけが使える）
create table attachment (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  session_id   uuid references agent_session(id) on delete cascade,
  name         text not null default '',
  content_type text not null check (content_type in ('image/png', 'image/jpeg', 'image/gif', 'image/webp')),
  size         int not null,
  data         bytea not null,
  created_at   timestamptz not null default now()
);
create index attachment_session_idx on attachment (session_id);

-- 画面に出す出来事（発言・ツールの呼び出しと結果・状態）. 画面は id より後のものを読む
create table session_event (
  id         bigserial primary key,
  session_id uuid not null references agent_session(id) on delete cascade,
  kind       text not null check (kind in ('user', 'assistant', 'tool_call', 'tool_result', 'status', 'error', 'note')),
  data       jsonb not null,
  created_at timestamptz not null default now()
);
create index session_event_session_idx on session_event (session_id, id);

-- 実行（利用者の発言1回ごと）. どのノードでも取れる. lease_until が切れたら、ほかのノードが続きから引き継ぐ.
-- 費用（ドル）は、応答ごとにトークン数とモデルの単価から計算して積み上げる
create table run (
  id               uuid primary key,
  session_id       uuid not null references agent_session(id) on delete cascade,
  status           text not null default 'queued' check (status in ('queued', 'running', 'done', 'failed', 'cancelled')),
  node             text,
  lease_until      timestamptz,
  cancel_requested boolean not null default false,
  attempts         int not null default 0,
  turns            int not null default 0,
  input_tokens     bigint not null default 0,
  output_tokens    bigint not null default 0,
  cache_read_tokens bigint not null default 0,
  cache_write_tokens bigint not null default 0,
  model            text not null default '',
  cost_usd         numeric(14, 6) not null default 0,
  error            text,
  created_at       timestamptz not null default now(),
  started_at       timestamptz,
  finished_at      timestamptz
);
create index run_queue_idx on run (created_at) where status in ('queued', 'running');
-- 1つの会話で同時に動く実行は1つだけ
create unique index run_active_idx on run (session_id) where status in ('queued', 'running');
create index run_created_idx on run (created_at);
-- 会話の最新の実行を引く（画面の表示・会話を消すときの連鎖）
create index run_session_idx on run (session_id, created_at desc);

-- ---------------------------------------------------------------- クレジット（Claude API の費用. ドル）

-- 台帳（会話を消しても残す. 使った額はここから数える. 会話を消して上限を戻せないように）
create table credit_usage (
  id         bigserial primary key,
  user_id    uuid references app_user(id) on delete set null,
  run_id     uuid,
  model      text not null,
  cost_usd   numeric(14, 6) not null,
  created_at timestamptz not null default now()
);
create index credit_usage_at_idx on credit_usage (created_at);
create index credit_usage_user_idx on credit_usage (user_id, created_at);

-- 利用者ごとの月の上限（行がなければ既定の上限. cap_usd が null なら上限なし）
create table user_budget (
  user_id    uuid primary key references app_user(id) on delete cascade,
  cap_usd    numeric(12, 2),
  updated_by uuid references app_user(id) on delete set null,
  updated_at timestamptz not null default now()
);

-- 動いているノード（心拍を10秒ごとに書く）. slots は同時に動かせる実行の数、active は動かしている数（空いているノードが先に取る）
create table node (
  name       text primary key,
  version    text not null,
  slots      int not null,
  active     int not null default 0,
  started_at timestamptz not null,
  seen_at    timestamptz not null
);

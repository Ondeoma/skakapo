# 開発用の環境変数（source scripts/dev-env.sh）。docker-compose.dev.yml の DB を使う
export DATABASE_URL=jdbc:postgresql://localhost:54410/skakapo DATABASE_USER=skakapo DATABASE_PASSWORD=skakapo
# 画面は Vite の開発サーバー（:5199. /api・/auth・/mcp を :8120 へ中継する）から開くので、公開 URL もそちらにする
export PUBLIC_URL=${PUBLIC_URL:-http://localhost:5199} HTTP_PORT=${HTTP_PORT:-8120}
# ノード: 同時に動かす実行の数・作業場所（リポジトリを取り出す場所）. scripts/dev-node.sh で2台目を足して、冗長構成を試せる
export WORKER_SLOTS=${WORKER_SLOTS:-2} WORK_DIR=${WORK_DIR:-work} NODE_NAME=${NODE_NAME:-node-1}
# Claude API のキーは、手元の環境変数 ANTHROPIC_API_KEY をそのまま使う（未設定ならエージェントは動かない）
# 認証: OIDC=dev なら開発用の IdP（scripts/dev-idp.sh、:9098）で OIDC のログインを試す。それ以外は名前だけの開発用ログイン
if [ "${OIDC:-}" = "dev" ]; then
  export OIDC_ISSUER=http://localhost:9098 OIDC_CLIENT_ID=skakapo OIDC_CLIENT_SECRET=skakapo-dev-secret-0123456789 OIDC_ALLOW_ANY_USER=true
  unset DEV_LOGIN
else
  export DEV_LOGIN=true
  unset OIDC_ISSUER OIDC_CLIENT_ID OIDC_CLIENT_SECRET
fi
# 秘密の値の暗号化の鍵（開発用. 本番では十分に長い乱数にする）
export APP_SECRET=${APP_SECRET:-skakapo-dev-app-secret-0123456789}
# 手元だけの秘密（ANTHROPIC_API_KEY など）は skakapo/.env に「KEY=値」で書く（.gitignore 済み. コミットしない）
skakapo_env_file="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")/.." && pwd)/.env"
if [ -f "$skakapo_env_file" ]; then set -a; . "$skakapo_env_file"; set +a; fi

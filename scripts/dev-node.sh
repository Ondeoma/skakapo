#!/usr/bin/env bash
# 手元で、ノードをもう1台動かす（冗長構成を試す. 実行は空いているノードが取り、止めたノードの実行はほかのノードが引き継ぐ）.
#   scripts/dev-node.sh node-2 8121
# sbt で動いているノード（scripts/dev-env.sh. node-1・:8120）と同じ DB を使う. 画面は :5199 から開く.
set -euo pipefail
cd "$(dirname "$0")/.."
name=${1:?ノードの名前を指定してください（例: node-2）}
port=${2:-8121}
cp=target/dev-classpath.txt
if [ ! -s "$cp" ] || [ -n "$(find modules -name '*.scala' -newer "$cp" -print -quit)" ]; then
  mkdir -p target
  sbt -batch -no-colors "export app/Runtime/fullClasspath" | tail -1 > "$cp"
fi
export HTTP_PORT=$port NODE_NAME=$name WORK_DIR=work/$name
. scripts/dev-env.sh
exec java -cp "$(cat "$cp")" skakapo.app.Main

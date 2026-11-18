<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*

// ノード（冗長構成）: どのノードが動いていて、いくつ実行を抱えているか. 実行は空いているノードが先に取る
case class State(stats: RV[Option[StatsDto]] = RV(None), timer: RV[Option[Int]] = RV(None))

def initState() = State()

def mounted(s: State): Unit = {
  load(s)
  s.timer := Some(org.scalajs.dom.window.setInterval(() => load(s), 10000))
}
def unmounted(s: State): Unit = s.timer.v.foreach(org.scalajs.dom.window.clearInterval)

def load(s: State): Unit = Store.report(Api.adminStats())(x => s.stats := Some(x))
def nodes(s: State) = s.stats.mapC(_.map(_.nodes).getOrElse(Nil))
def summary(s: State) = s.stats.mapC(_.map(x => s"順番待ち ${x.queued}・作業中 ${x.running}・24時間の実行 ${x.runs24h}・24時間のトークン ${Fmt.tokens(x.tokens24h)}").getOrElse(""))
def slots(n: NodeDto): String = s"${n.active} / ${n.slots}"
</script>

<template>
  <div class="panel">
    <p>{{ summary(s) }}</p>
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>ノード</th><th>状態</th><th>実行中 / 枠</th><th>版</th><th>起動</th><th>最後の心拍</th></tr></thead>
        <tbody>
          <tr v-for="n in nodes(s)">
            <td>{{ n.name }}</td>
            <td><span :class='if (n.alive) "st st-done" else "st st-failed"'>{{ if (n.alive) "動いている" else "止まっている" }}</span></td>
            <td class="num">{{ slots(n) }}</td>
            <td>{{ n.version }}</td>
            <td>{{ Fmt.dateTime(n.startedAt) }}</td>
            <td>{{ Fmt.ago(n.seenAt) }}</td>
          </tr>
        </tbody>
      </table>
    </div>
    <p class="small muted">ノードを足すと、実行を取り合って分担します（WORKER_SLOTS で1台あたりの同時実行数）。止まったノードの実行は、ほかのノードが続きから引き継ぎます。</p>
  </div>
</template>

<script lang="scala">
import skakapo.web.*
import skakapo.shared.*
import org.scalajs.dom

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 連携: エージェントがあなたの権限で各サービスを読み書きできるよう、各サービスにトークンを発行してもらう（OAuth）.
// OAuth に対応していないサービスは、そのサービスで発行した API トークンを貼り付ける
case class State(list: RV[List[ConnectionDto]] = RV(Nil), tokenFor: RV[Option[UUID]] = RV(None), token: RV[String] = RV(""),
                 busy: RV[Boolean] = RV(false))

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("連携")
  load(s)
  Router.query.v.get("connected").foreach(k => Toasts.show(s"$k と連携しました"))
}

def load(s: State): Unit = Toasts.report(Api.connections())(xs => s.list := xs)

def oauth(c: ConnectionDto): Unit = Toasts.report(Api.connectOAuth(c.service.id, "/connections"))(url => dom.window.location.href = url)

def saveToken(s: State): Unit = s.tokenFor.v.foreach { id =>
  s.busy := true
  Api.connectToken(id, s.token.v).onComplete { r =>
    s.busy := false
    r.fold(Toasts.error, _ => { s.tokenFor := None; s.token := ""; Toasts.show("連携しました"); load(s) })
  }
}

def disconnect(s: State, c: ConnectionDto): Unit = Dialog.confirm(s"${c.service.name} との連携をやめますか？（エージェントは ${c.service.name} を使えなくなります）", "やめる") {
  Toasts.report(Api.disconnect(c.service.id))(_ => load(s))
}

def refresh(c: ConnectionDto): Unit = Toasts.report(Api.refreshTools(c.service.id))(n => Toasts.show(s"${c.service.name} のツールを読み直しました（$n 個）"))

def kind(c: ConnectionDto): String = ServiceKind.label(c.service.kind)
def via(c: ConnectionDto): String = c.via match {
  case Some("oauth")  => "OAuth で連携"
  case Some("manual") => "API トークンで連携"
  case _              => ""
}
def canToken(c: ConnectionDto): Boolean = c.service.manualToken
def tokenOpen(s: State, c: ConnectionDto) = s.tokenFor.mapC(_.contains(c.service.id))
</script>

<template>
  <div class="page narrow">
    <h1>連携</h1>
    <p class="muted">エージェントは、ここで連携したサービスを、あなたの権限で読み書きします。各サービスで許可すると、トークンが {{ AppNames.self }} に暗号化して保存されます（連携をやめると削除されます）。</p>
    <div v-if="s.list.mapC(_.isEmpty)" class="empty">連携できるサービスがありません（管理者がサービスを設定します）。</div>
    <div class="cards">
      <div v-for="c in s.list" :key="c.service.id.toString" class="card svc">
        <div class="row top">
          <b class="grow">{{ c.service.name }} <span class="small muted">{{ kind(c) }}</span></b>
          <span v-if="c.connected" class="st st-done">連携済み</span>
          <span v-else-if="c.service.hasAgentToken" class="st st-idle">専用アカウントで利用</span>
          <span v-else class="st">未連携</span>
        </div>
        <p class="small muted desc">{{ c.service.description }}</p>
        <p v-if="!c.connected && c.service.hasAgentToken" class="small muted">連携しなくても、エージェントは管理者が設定した専用アカウントで使えます。あなたの権限で使わせたいときは連携してください。</p>
        <p v-if="c.connected" class="small muted">{{ via(c) }}・{{ c.connectedAt.map(Fmt.dateTime).getOrElse("") }}{{ if (c.scope.nonEmpty) s"・スコープ ${c.scope}" else "" }}</p>
        <div class="row actions">
          <template v-if="c.connected">
            <button v-if="c.service.mcpUrl.nonEmpty" class="ghost small" @click="refresh(c)">ツールを読み直す</button>
            <button class="ghost small danger" @click="disconnect(s, c)">連携をやめる</button>
          </template>
          <template v-else>
            <button v-if="c.canOAuth" class="primary small" @click="oauth(c)">連携</button>
            <button v-if="canToken(c)" class="ghost small" @click="s.tokenFor := Some(c.service.id)">API トークンで連携</button>
            <span v-if="!c.canOAuth && !canToken(c)" class="small muted">管理者が OAuth を設定すると連携できます</span>
          </template>
        </div>
        <form v-if="tokenOpen(s, c)" class="row tok" @submit.prevent="saveToken(s)">
          <input type="password" class="grow" v-model="s.token" placeholder="そのサービスの設定で発行した API トークン" autocomplete="off">
          <button type="submit" class="primary small" :disabled="s.busy.v">保存</button>
          <button type="button" class="ghost small" @click="s.tokenFor := None">やめる</button>
        </form>
      </div>
    </div>
  </div>
</template>

<style scoped>
.cards { display: flex; flex-direction: column; gap: .6rem; margin-top: 1rem; }
.svc { padding: .8rem 1rem; }
.top { align-items: center; gap: .6rem; }
.desc { margin: .3rem 0; }
.actions { gap: .5rem; margin-top: .4rem; flex-wrap: wrap; }
.tok { gap: .5rem; margin-top: .6rem; }
</style>

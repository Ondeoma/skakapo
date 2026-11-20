<script lang="scala">
import skakapo.web.*
import skakapo.web.admin.*

// 管理画面: エージェントの前提とモデル・連携するサービス・リポジトリ・依頼の種類（最初のフォームと手順）・ノード
case class State(tab: RV[String] = RV("agent"))

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("管理")
  Router.query.v.get("tab").foreach(t => s.tab := t)
}

val tabs: List[(String, String)] = List("agent" -> "エージェント", "credits" -> "クレジット", "services" -> "サービス", "repos" -> "リポジトリ", "clouds" -> "クラウド", "groups" -> "利用者の種類",
  "types" -> "依頼の種類", "nodes" -> "ノード")
def tabClass(s: State, t: String) = s.tab.mapC(c => if (c == t) "active" else "")
def isAdmin = Store.me.mapC(_.exists(_.isAdmin))
</script>

<template>
  <div class="page wide">
    <h1>管理</h1>
    <p v-if="!isAdmin.v" class="error">管理者だけが使えます。</p>
    <template v-else>
      <div class="tabs">
        <button v-for="t in tabs" :class="tabClass(s, t._1)" @click="s.tab := t._1">{{ t._2 }}</button>
      </div>
      <AgentTab v-if='s.tab.mapC(_ == "agent")' />
      <CreditsTab v-if='s.tab.mapC(_ == "credits")' />
      <ServicesTab v-if='s.tab.mapC(_ == "services")' />
      <ReposTab v-if='s.tab.mapC(_ == "repos")' />
      <CloudTab v-if='s.tab.mapC(_ == "clouds")' />
      <GroupsTab v-if='s.tab.mapC(_ == "groups")' />
      <TypesTab v-if='s.tab.mapC(_ == "types")' />
      <NodesTab v-if='s.tab.mapC(_ == "nodes")' />
    </template>
  </div>
</template>

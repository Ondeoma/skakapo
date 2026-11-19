<script lang="scala">
import io.github.ondeoma.scalactive.components.routes.RouterComponent
import org.scalajs.dom
import skakapo.shared.*
import skakapo.web.parts.*
import skakapo.web.views.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

// 画面全体の枠: ヘッダー（ナビゲーション・メニュー）と、ルートごとの画面
case class State(ready: RV[Boolean] = RV(false), menu: RV[Boolean] = RV(false), nav: RV[Boolean] = RV(false))

def initState() = State()

val routes: PathPatternRoutes = List(
  ("/", _ => HomeView()),
  ("/new", _ => NewRequestView()),
  ("/sessions/:id", r => SessionView(sessionId = UUID.fromString(r.pathname.groups("id")))),
  ("/connections", _ => ConnectionsView()),
  ("/admin", _ => AdminView()),
  ("/login", _ => LoginView()),
  ("/settings", _ => SettingsView()),
)

def mounted(s: State): Unit = {
  Fallback.install(routes)
  Api.clientConfig().foreach(c => Store.config := Some(c))
  val path = dom.window.location.pathname
  if (path.startsWith("/login")) s.ready := true
  else Store.report(Api.me()) { me =>
    Store.me := Some(me)
    AppNames.set(me.appNames)
    Observe.setUser(me.id.toString)
    Store.start()
    s.ready := true
  }
}

// ログアウトするときは、この端末のプッシュ通知もやめる（ログアウトした端末に会話の様子を送らない）
def logout(): Unit = Store.report(Pwa.disable().recover { case _ => "off" }.flatMap(_ => Api.logout()))(url => dom.window.location.href = url)

def loggedIn = Store.me.mapC(_.isDefined)
def meBrief = Store.me.mapC(_.map(_.brief))
def userName = Store.me.mapC(_.map(_.name).getOrElse(""))
def userEmail = Store.me.mapC(_.flatMap(_.email).getOrElse(""))
def loadingClass = Store.loading.mapC(l => if (l) "progress on" else "progress")
def busy = Store.busyCount
def hasBusy = busy.mapC(_ > 0)
def isAdmin = Store.me.mapC(_.exists(_.isAdmin))

// ヘッダーのクレジットの表示: 上限があれば自分の残り、なければ今月使った額. 残りが2割を切ったら注意の色
def creditText = Store.credit.mapC(_.fold("")(c => c.userRemaining.fold(s"今月 ${Usd.fmt(c.userUsed)}")(r => s"残り ${Usd.fmt(r)}")))
def creditClass = Store.credit.mapC(_.fold("credit") { c =>
  val low = c.userCap.exists(cap => c.userRemaining.exists(_ <= cap * 0.2)) || c.globalBudget.exists(b => c.globalRemaining.exists(_ <= b * 0.2))
  if (c.blocked.isDefined) "credit out" else if (low) "credit low" else "credit"
})
def creditTitle = Store.credit.mapC(_.fold("") { c =>
  val mine = c.userCap.fold(s"あなた: 今月 ${Usd.fmt(c.userUsed)} 使用（上限なし）")(cap => s"あなた: 残り ${Usd.fmt(c.userRemaining.getOrElse(0))} / 上限 ${Usd.fmt(cap)}（今月 ${Usd.fmt(c.userUsed)} 使用）")
  val all = c.globalBudget.fold(s"全体: 今月 ${Usd.fmt(c.globalUsed)} 使用（予算なし）")(b => s"全体: 残り ${Usd.fmt(c.globalRemaining.getOrElse(0))} / 予算 ${Usd.fmt(b)}（今月 ${Usd.fmt(c.globalUsed)} 使用）")
  s"$mine\n$all"
})
def creditLines = creditTitle.mapC(_.split("\n").toList.filter(_.nonEmpty))

def nav = Store.me.mapC(m => List("/" -> "依頼", "/connections" -> "連携") ++ (if (m.exists(_.isAdmin)) List("/admin" -> "管理") else Nil))
def navClass(p: String) = Router.href.mapC { h =>
  val path = new dom.URL(h).pathname
  if (path == p || (p == "/" && (path.startsWith("/sessions") || path == "/new")) || (p != "/" && path.startsWith(p))) "nav active" else "nav"
}
</script>

<template>
  <header v-if="loggedIn" class="top">
    <button class="ghost icon only-sp" title="メニュー" @click="s.nav := !s.nav.v">☰</button>
    <RouterLink to="/" class="logo"><span class="brand"><img :src='AppNames.logo("/logo.svg").v' alt="" height="32" class="brand-logo"><span class="hide-sp">{{ AppNames.self }}</span></span></RouterLink>
    <nav class="navs hide-sp">
      <RouterLink v-for="n in nav" :to="n._1" :class="navClass(n._1)">{{ n._2 }}<span v-if='n._1 == "/" && hasBusy.v' class="badge">{{ busy }}</span></RouterLink>
    </nav>
    <div class="spacer"></div>
    <span v-if="creditText.v.nonEmpty" :class="creditClass" :title="creditTitle">{{ creditText }}</span>
    <div class="me">
      <button class="ghost me-btn" @click="s.menu := !s.menu.v"><Avatar :user="meBrief.v" :size="30" /></button>
      <div v-if="s.menu" class="menu-bg" @click="s.menu := false"></div>
      <div v-if="s.menu" class="menu" @click="s.menu := false">
        <div class="who"><b>{{ userName }}</b><span class="muted small">{{ userEmail }}</span></div>
        <div v-if="creditLines.v.nonEmpty" class="who small"><span v-for="l in creditLines" class="muted">{{ l }}</span></div>
        <RouterLink to="/settings">設定</RouterLink>
        <a href="#" @click.prevent="logout()">ログアウト</a>
      </div>
    </div>
    <div :class="loadingClass"></div>
  </header>
  <div v-if="s.nav" class="sp-nav only-sp" @click="s.nav := false">
    <RouterLink v-for="n in nav" :to="n._1" :class="navClass(n._1)">{{ n._2 }}</RouterLink>
  </div>
  <main class="main">
    <RouterComponent v-if="s.ready" :routes="routes" />
  </main>
  <DialogHost />
  <div class="toasts">
    <div v-for="t in Store.toasts" :key="t.id" :class='if (t.error) "toast error" else "toast"'>
      <span class="grow">{{ t.text }}</span>
      <button @click="Store.dismiss(t.id)">×</button>
    </div>
  </div>
</template>

<style scoped>
.top { position: sticky; top: 0; z-index: 100; height: var(--header); display: flex; align-items: center; gap: .5rem; padding: 0 .8rem;
  /* ガラスの帯. 下端にグラデーションの細い線 */
  background: color-mix(in srgb, var(--bg) 72%, transparent); backdrop-filter: var(--blur); -webkit-backdrop-filter: var(--blur);
  border-bottom: 1px solid var(--line); }
.top::after { content: ""; position: absolute; left: 0; right: 0; bottom: -1px; height: 1px; background: var(--grad); opacity: .55; pointer-events: none; }
.brand { font-weight: 700; font-size: 1.25rem; letter-spacing: .04em; color: var(--fg); display: inline-flex; align-items: center; gap: .5rem; white-space: nowrap; margin-right: 1rem; }
.brand img { display: block; }
.brand-logo { width: auto; }
.icon { font-size: 1.2rem; padding: .3rem .55rem; }
.spacer { flex: 1; }
.credit { --c: var(--accent); font-family: var(--mono); font-size: .74rem; letter-spacing: .03em; padding: .15rem .65rem; border-radius: 999px; white-space: nowrap; cursor: default;
  color: var(--c); background: color-mix(in srgb, var(--c) 12%, transparent); border: 1px solid color-mix(in srgb, var(--c) 32%, transparent); }
.credit.low { --c: var(--warn); }
.credit.out { --c: var(--danger); }
.me { position: relative; }
.me-btn { padding: .15rem; border-radius: 999px; }
.menu { position: absolute; right: 0; top: 2.8rem; background: var(--surface); border: 1px solid var(--line); border-radius: 10px; box-shadow: var(--shadow);
  min-width: 220px; padding: .3rem; display: flex; flex-direction: column; z-index: 200; }
.menu-bg { position: fixed; inset: 0; z-index: 199; }
.who { display: flex; flex-direction: column; padding: .5rem .7rem; border-bottom: 1px solid var(--line); margin-bottom: .3rem; }
.progress { position: absolute; left: 0; bottom: -1px; height: 2px; width: 0; background: var(--grad); z-index: 1; opacity: 0; transition: opacity .2s; }
.progress.on { width: 100%; opacity: .7; animation: prog 1.2s ease-in-out infinite; }
@keyframes prog { from { transform: scaleX(0); transform-origin: left; } to { transform: scaleX(1); transform-origin: left; } }
.sp-nav { position: fixed; z-index: 150; top: var(--header); left: 0; right: 0; background: var(--surface); border-bottom: 1px solid var(--line);
  display: flex; flex-direction: column; padding: .4rem; box-shadow: var(--shadow); }
</style>

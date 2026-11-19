<script lang="scala">
import skakapo.web.*
import skakapo.shared.*


// 会話（依頼）の一覧. 「新しい依頼」からフォームで始める. アーカイブした会話は「アーカイブ済み」で見る
case class State(archived: RV[Boolean] = RV(false), old: RV[List[SessionDto]] = RV(Nil))

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("依頼")
  Store.refresh()
}

def toggle(s: State): Unit = {
  s.archived := !s.archived.v
  reload(s)
}

/** アーカイブ済みを見ているときは、その一覧を読み直す（いつもの一覧は Store.refresh で読み直る）. */
def reload(s: State): Unit = if (s.archived.v) Store.report(Api.sessions(archived = true))(xs => s.old := xs)

def archive(s: State, x: SessionDto): Unit = SessionActions.archive(x.id, !x.archived)(reload(s))
def remove(s: State, x: SessionDto): Unit = SessionActions.remove(x.id, x.title)(reload(s))

def list(s: State) = computed(if (s.archived.v) s.old.v else Store.sessions.v)
def empty(s: State) = list(s).mapC(_.isEmpty)
def statusText(x: SessionDto): String = SessionStatus.label(x.status)
def agentOff = Store.config.mapC(_.exists(c => !c.agentReady))
def blocked = Store.credit.mapC(_.flatMap(_.blocked).getOrElse(""))
def creditRows = Store.credit.mapC(_.toList.flatMap { c =>
  List(
    ("あなた", c.userCap.fold(s"上限なし（今月 ${Usd.fmt(c.userUsed)} 使用）")(cap => s"残り ${Usd.fmt(c.userRemaining.getOrElse(0))} / ${Usd.fmt(cap)}"), ratio(c.userUsed, c.userCap)),
    ("全体", c.globalBudget.fold(s"予算なし（今月 ${Usd.fmt(c.globalUsed)} 使用）")(b => s"残り ${Usd.fmt(c.globalRemaining.getOrElse(0))} / ${Usd.fmt(b)}"), ratio(c.globalUsed, c.globalBudget)))
})
/** 使った割合（0〜100. 上限がなければ -1） */
def ratio(used: Double, cap: Option[Double]): Int = cap.fold(-1)(c => if (c <= 0) 100 else math.min(100, math.round(used / c * 100).toInt))
def barStyle(p: Int): String = s"width: ${math.max(p, 0)}%"
def barClass(p: Int): String = if (p >= 100) "fill out" else if (p >= 80) "fill low" else "fill"
</script>

<template>
  <div class="page">
    <div class="row head">
      <h1 class="grow">依頼</h1>
      <button class="ghost small" @click="toggle(s)">{{ if (s.archived.v) "← 依頼に戻る" else "アーカイブ済み" }}</button>
      <RouterLink to="/new" class="btn primary">＋ 新しい依頼</RouterLink>
    </div>
    <p v-if="agentOff" class="panel warn">Claude API のキーが設定されていないため、エージェントはまだ動きません（管理者に連絡してください）。</p>
    <p v-if="blocked.v.nonEmpty" class="panel warn">{{ blocked }}</p>
    <div v-if="creditRows.v.nonEmpty" class="panel credits">
      <div v-for="r in creditRows" class="crow">
        <span class="who">{{ r._1 }}</span>
        <div class="bar"><div :class="barClass(r._3)" :style="barStyle(r._3)"></div></div>
        <span class="small">{{ r._2 }}</span>
      </div>
      <p class="small muted note">今月の Claude API の費用（{{ AppNames.self }} がトークン数とモデルの単価から計算したもの）。毎月1日（UTC）に戻ります。</p>
    </div>
    <div v-if="empty(s)" class="empty">
      <p v-if="s.archived">アーカイブした依頼はありません。</p>
      <template v-else>
        <p>まだ依頼はありません。</p>
        <p class="small muted">不具合の修正・機能の追加・調査などを、sBirds シリーズのサービスと Git のリポジトリを使ってエージェントが進めます。</p>
        <p><RouterLink to="/new" class="btn primary">＋ 新しい依頼</RouterLink></p>
      </template>
    </div>
    <div v-else class="cards">
      <div v-for="x in list(s)" :key="x.id.toString" class="card item">
        <!-- カード全体を押すと開く（ボタンはその上に重ねる） -->
        <RouterLink :to='"/sessions/" + x.id' class="card-cover" :aria-label="x.title"></RouterLink>
        <div class="row top">
          <span :class="Fmt.statusClass(x.status)">{{ statusText(x) }}</span>
          <span class="small muted">{{ x.requestLabel }}</span>
          <span class="grow"></span>
          <span class="small muted">{{ Fmt.ago(x.updatedAt) }}</span>
        </div>
        <div class="row bottom">
          <div class="grow">
            <div class="title">{{ x.title }}</div>
            <div v-if="x.links.nonEmpty" class="small muted">作ったもの {{ x.links.size }} 件</div>
          </div>
          <div class="acts">
            <button class="ghost small" @click="archive(s, x)">{{ if (x.archived) "アーカイブを解除" else "アーカイブ" }}</button>
            <button class="ghost small danger" @click="remove(s, x)">削除</button>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.head { align-items: center; gap: .6rem; margin-bottom: .8rem; }
.head h1 { margin: 0; }
.cards { display: flex; flex-direction: column; gap: .5rem; }
.item { position: relative; color: var(--fg); padding: .75rem 1rem; }
.item:hover { border-color: var(--accent); }
.bottom { align-items: flex-end; gap: .6rem; }
/* ボタンはカードを開くリンクの上に重ねる. マウスでは乗せたときだけ見せる */
.acts { position: relative; z-index: 1; display: flex; gap: .3rem; }
@media (hover: hover) {
  .acts { opacity: 0; transition: opacity .15s; }
  .item:hover .acts, .acts:focus-within { opacity: 1; }
}
.top { align-items: center; gap: .6rem; }
.title { font-weight: 600; margin: .3rem 0 .1rem; color: var(--fg); }
.warn { border-color: var(--warn); }
.credits { display: flex; flex-direction: column; gap: .35rem; margin-bottom: .8rem; padding: .7rem 1rem; }
.crow { display: grid; grid-template-columns: 3.5rem minmax(80px, 1fr) auto; gap: .7rem; align-items: center; }
.crow .who { font-size: .85rem; font-weight: 600; }
.bar { height: 6px; border-radius: 999px; background: var(--hover); overflow: hidden; box-shadow: inset 0 0 0 1px var(--line); }
.fill { height: 100%; border-radius: 999px; background: var(--grad); box-shadow: 0 0 10px color-mix(in srgb, var(--accent) 45%, transparent); }
.fill.low { background: var(--warn); }
.fill.out { background: var(--danger); }
.note { margin: .2rem 0 0; }
</style>

<script lang="scala">
import skakapo.web.*
import skakapo.web.parts.*
import skakapo.shared.*

// クレジット（Claude API の費用. ドル）: 全体の月の予算・利用者ごとの上限の既定と、人ごとの上限. 今月の使った額
case class Edit(user: UserCreditDto, mode: RV[String], amount: RV[String])

case class State(settings: RV[Option[AdminSettingsDto]] = RV(None), budget: RV[String] = RV(""), defaultCap: RV[String] = RV(""),
                 used: RV[Double] = RV(0), month: RV[String] = RV(""), users: RV[List[UserCreditDto]] = RV(Nil), edit: RV[Option[Edit]] = RV(None))

def initState() = State()

def mounted(s: State): Unit = load(s)

def load(s: State): Unit = {
  Toasts.report(Api.adminSettings()) { d =>
    s.settings := Some(d)
    s.budget := d.agent.monthlyBudgetUsd.fold("")(amount)
    s.defaultCap := d.agent.defaultUserCapUsd.fold("")(amount)
  }
  Toasts.report(Api.adminCredits()) { c =>
    s.used := c.used
    s.month := c.month
    s.users := c.users
  }
}

def amount(v: Double): String = Usd.fmt(v).drop(1)
/** 空なら決めない（None）. 数字でなければ Left */
def parse(v: String): Either[String, Option[Double]] =
  if (v.trim.isEmpty) Right(None) else v.trim.stripPrefix("$").toDoubleOption.filter(_ >= 0).map(Some(_)).toRight("金額は 0 以上の数で入れてください")

def saveLimits(s: State): Unit = s.settings.v.foreach { d =>
  (parse(s.budget.v), parse(s.defaultCap.v)) match {
    case (Right(b), Right(c)) =>
      Toasts.report(Api.saveAdminSettings(AdminSettingsReq(d.agent.copy(monthlyBudgetUsd = b, defaultUserCapUsd = c), d.trackers))) { _ =>
        Toasts.show("保存しました"); load(s); Store.refresh()
      }
    case (Left(e), _) => Toasts.show(e, error = true)
    case (_, Left(e)) => Toasts.show(e, error = true)
  }
}

def open(s: State, u: UserCreditDto): Unit = s.edit := Some(Edit(u,
  RV(u.capOverride match { case None => "default"; case Some(None) => "none"; case Some(Some(_)) => "amount" }),
  RV(u.capOverride.flatten.fold("")(amount))))

def saveCap(s: State, e: Edit): Unit = {
  val req = e.mode.v match {
    case "default" => Right(UserCapReq(useDefault = true))
    case "none"    => Right(UserCapReq(useDefault = false, None))
    case _         => parse(e.amount.v).flatMap(_.toRight("金額を入れてください")).map(v => UserCapReq(useDefault = false, Some(v)))
  }
  req match {
    case Left(err) => Toasts.show(err, error = true)
    case Right(r) => Toasts.report(Api.setUserCap(e.user.user.id, r))(_ => { s.edit := None; load(s); Store.refresh() })
  }
}

def edits(s: State) = s.edit.mapC(_.toList)
def capText(u: UserCreditDto): String = u.cap.fold("上限なし")(Usd.fmt) + (if (u.capOverride.isDefined) "（個別）" else "（既定）")
def remainText(u: UserCreditDto): String = u.cap.fold("—")(c => Usd.fmt(math.max(c - u.used, 0)))
def totalText(s: State) = computed {
  val b = s.settings.v.flatMap(_.agent.monthlyBudgetUsd)
  s"${s.month.v} の全体: ${Usd.fmt(s.used.v)} 使用" + b.fold("（予算なし）")(x => s" / 予算 ${Usd.fmt(x)}（残り ${Usd.fmt(math.max(x - s.used.v, 0))}）")
}
def defaultText(s: State) = s.settings.mapC(_.flatMap(_.agent.defaultUserCapUsd).fold("上限なし")(Usd.fmt))
</script>

<template>
  <form class="panel" @submit.prevent="saveLimits(s)">
    <p><b>{{ totalText(s) }}</b></p>
    <p class="small muted">金額は、{{ AppNames.self }} が実行ごとにトークン数とモデルの単価から計算した Claude API の費用です（Anthropic のアカウントの残高とは別）。月は UTC で区切ります。上限・予算に達すると、新しい依頼と返事を断り、動いている作業も次のターンの前で止めます。</p>
    <div class="row">
      <label class="field"><span>全体の月の予算（ドル. 空なら決めない）</span><input type="text" inputmode="decimal" v-model="s.budget" placeholder="例: 300"></label>
      <label class="field"><span>利用者ごとの月の上限の既定（ドル. 空なら上限なし）</span><input type="text" inputmode="decimal" v-model="s.defaultCap" placeholder="例: 20"></label>
      <button type="submit" class="primary save">保存</button>
    </div>
  </form>
  <div class="panel">
    <div class="table-wrap">
      <table class="list">
        <thead><tr><th>利用者</th><th class="num">今月の使用</th><th>上限</th><th class="num">残り</th><th class="num">実行</th><th></th></tr></thead>
        <tbody>
          <tr v-for="u in s.users" class="click" @click="open(s, u)">
            <td><span class="who"><Avatar :user="Some(u.user.person)" :size="22" /> {{ u.user.name }}</span></td>
            <td class="num">{{ Usd.fmt(u.used) }}</td>
            <td>{{ capText(u) }}</td>
            <td class="num">{{ remainText(u) }}</td>
            <td class="num">{{ u.runs.toString }}</td>
            <td><button class="ghost small">上限変更</button></td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
  <Modal v-for="e in edits(s)" :onClose="_ => s.edit := None">
    <h2>{{ e.user.user.name }} の月の上限</h2>
    <form @submit.prevent="saveCap(s, e)">
      <label class="row"><input type="radio" name="cap" value="default" :checked='e.mode.v == "default"' @change='e.mode := "default"'> 既定（{{ defaultText(s) }}）</label>
      <label class="row"><input type="radio" name="cap" value="none" :checked='e.mode.v == "none"' @change='e.mode := "none"'> 上限なし</label>
      <label class="row"><input type="radio" name="cap" value="amount" :checked='e.mode.v == "amount"' @change='e.mode := "amount"'> 金額を決める
        <input type="text" inputmode="decimal" v-model="e.amount" placeholder="例: 50" class="amount" :disabled='e.mode.v != "amount"'> ドル</label>
      <p class="small muted">今月の使用: {{ Usd.fmt(e.user.used) }}</p>
      <div class="actions"><button type="button" @click="s.edit := None">やめる</button><button type="submit" class="primary">保存</button></div>
    </form>
  </Modal>
</template>

<style scoped>
.row { align-items: flex-end; gap: .8rem; flex-wrap: wrap; }
.save { margin-bottom: .6rem; }
.who { display: inline-flex; align-items: center; gap: .4rem; }
.amount { width: 7rem; margin-left: .4rem; }
</style>

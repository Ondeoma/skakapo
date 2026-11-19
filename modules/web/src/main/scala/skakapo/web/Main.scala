package skakapo.web

import io.github.ondeoma.scalactive.ScalactiveConfig
import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom

/**
 * 画面の入口.
 * `@main def main()` にすると生成される `main` クラスと `Main` が、大文字小文字を区別しないファイルシステム（macOS）で
 * 同じファイル名になり差分コンパイルが壊れるため、通常の main(args) にする.
 */
object Main {

  def main(args: Array[String]): Unit = {
    ScalactiveConfig.titleTemplate = "%s | skakapo"
    ScalactiveConfig.defaultTitle = Some("skakapo")
    AppNames.init("skakapo")
    Theme.apply()
    // skorimako のブラウザの観測（設定があるときだけ）と、PWA（ホーム画面に追加・オフラインで開く・プッシュ通知）
    // 共通部品（Pwa・Observe）からの更新の要求にも、CSRF 対策の X-Skakapo を付ける
    sbirds.web.Fetch.extraHeaders = Map("X-Skakapo" -> "1")
    Observe.start()
    Pwa.register()
    App()(dom.document.body, AddMethod.appendS(IdSelector("app"))).init()
  }

}

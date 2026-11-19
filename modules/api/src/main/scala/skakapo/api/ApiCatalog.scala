package skakapo.api

/** REST API の一覧（MCP の list_api_endpoints と GET /api/endpoints）. */
object ApiCatalog {
  final case class Endpoint(method: String, path: String, description: String)
  val all: List[Endpoint] = List(
    Endpoint("GET", "/api/me", "自分の情報"),
    Endpoint("GET", "/api/me/credit", "今月のクレジット（自分の使った額・上限・残り、全体の予算と残り）"),
    Endpoint("GET", "/api/me/context", "自分用の前提（エージェントに最初に読ませる文）"),
    Endpoint("PUT", "/api/me/context", "自分用の前提を変える {text}"),
    Endpoint("GET", "/api/request-types", "依頼の種類と、そのフォームの項目"),
    Endpoint("GET", "/api/sessions?archived=false", "自分の会話の一覧（新しい順）"),
    Endpoint("POST", "/api/sessions", "依頼を始める {requestType, values: {項目: 値}, attachments: [添付の id]}（エージェントが裏で作業を始める）"),
    Endpoint("GET", "/api/sessions/{id}", "会話の様子（出来事・実行の状態）"),
    Endpoint("GET", "/api/sessions/{id}/events?after={id}&wait={秒}", "新しい出来事（wait 秒まで待つ）"),
    Endpoint("POST", "/api/sessions/{id}/messages", "返事をする {text, attachments: [添付の id]}（文か添付のどちらかは要る）"),
    Endpoint("POST", "/api/sessions/{id}/cancel", "作業を止める"),
    Endpoint("PATCH", "/api/sessions/{id}", "会話をアーカイブ・アーカイブの解除・題名の変更 {archived?, title?}"),
    Endpoint(
      "POST",
      "/api/attachments",
      "ファイルを添付する（?name=ファイル名. 本文がファイル. 種類は問わない. 10MB まで. 画像は 5MB まで. 依頼を始める・返事を送る前に）"
    ),
    Endpoint("GET", "/api/attachments/{id}", "添付したファイル"),
    Endpoint("DELETE", "/api/sessions/{id}", "会話を削除する"),
    Endpoint("GET", "/api/connections", "連携できるサービスと、連携の状態"),
    Endpoint("PUT", "/api/connections/{serviceId}/token", "API トークンで連携する {token}"),
    Endpoint("DELETE", "/api/connections/{serviceId}", "連携をやめる")
    // 管理の API は画面のセッションからだけ使える（API トークンでは使えない）ので、ここには載せない
  )
}

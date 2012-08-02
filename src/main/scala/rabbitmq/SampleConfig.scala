package rabbitmq

/**
 * サンプル用の rabbitmq の設定を保持します
 */
case class SampleConfig() {
  val hosts: Seq[String] = Seq(
    "xx.xx.xx.xx",
    "xx.xx.xx.xx"
  )
}

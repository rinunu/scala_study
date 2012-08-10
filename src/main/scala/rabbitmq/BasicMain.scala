/**
 * RabbitMQ の基本的な使い方です
 */

package rabbitmq

import rabbitmq.Implicits._
import com.twitter.util.FuturePool
import java.util.concurrent.Executors
import grizzled.slf4j.Logging
import com.rabbitmq.client.AMQP.BasicProperties
import rabbitmq.ConnectionFactory
import com.rabbitmq.client._

/**
 * MQ へ publish する人です
 */
case class Producer(connectionFactory: ConnectionFactory,
                    exchange: Exchange,
                    routingKey: RoutingKey) extends Logging {

  def apply() {
    val connection = connectionFactory.connect()
    val channel = connection.createChannel()

    // confirm(ack を有効にします)
    channel.confirmSelect()

    // コールバックは connection のメインスレッドで実行されます

    // basic.return を受け取ります(mandatory なメッセージが失敗した場合に)
    // ack が戻る前に呼び出されます
    // 例: キューが存在しない場合など
    // これを受け取った場合、投入するキューを変更するなどの対応が必要です
    channel.addReturnListener(new ReturnListener {
      def handleReturn(replyCode: Int, replyText: String, exchange: String, routingKey: String, properties: BasicProperties, body: Array[Byte]) {
        debug("return")
      }
    })

    // 試した感じでは、 shutdown を検知した際に、まっさきに実行されれるようです。
    // たとえば、 channel.close で検知した場合、 close が戻るよりも先に、こちらが呼ばれました。
    // が、絶対ではなく、 close が戻った後に呼ばれたこともありました。
    channel.addShutdownListener(new ShutdownListener {
      def shutdownCompleted(cause: ShutdownSignalException) {
        debug("shutdown")
      }
    })

    for (no <- 0 to 10000) {
      publish(channel, no)
      Thread.sleep(1000)
    }
  }

  /**
   * publish します
   *
   * ack が通知されるまでブロックします
   *
   * @throws TimeoutException
   */
  def publish(channel: Channel, no: Int) {
    try {
      val mandatory = true // キューに届かなかった場合に basic.return が通知されます
      val immediate = false
      val body = "test-" + no

      debug("publish start: " + body)
      channel.basicPublish(
        exchange.name,
        routingKey.value,
        mandatory,
        immediate,
        null,
        body.getBytes("UTF-8"))

      // ack を待ちます。 スレッド毎にチャンネルを持つように実装しているので、他処理のことは気にせず、ブロッキングしちゃいます。
      // basic.return 時には、 basic.return => ack の順に通知されます(つまり成功とみなされます)。
      channel.waitForConfirmsOrDie(1000)
    } catch {
      case e => debug(e)
      throw e
    } finally {
      debug("publish end")
    }
  }
}

/**
 * MQ から consume する人です
 */
case class Consumer(connectionFactory: ConnectionFactory,
                    queue: Queue
                     ) extends Logging {

  def apply() {
    val connection = connectionFactory.connect()
    try {
      val channel = connection.createChannel()

      // コールバックは connection のメインスレッドで実行されます
      channel.addShutdownListener(new ShutdownListener {
        def shutdownCompleted(cause: ShutdownSignalException) {
          debug("shutdown")
        }
      })

      trace("start " + queue)
      val consumer = new QueueingConsumer(channel)
      channel.basicConsume(queue.name, consumer)
      while (true) {
        val delivery = consumer.nextDelivery()
        debug("consume: " + delivery.string)
        channel.basicAck(delivery.tag, false)
      }
    } catch {
      case e =>
        debug(e)
        throw e
    } finally {
      debug("finally")
      connection.close()
    }
  }
}

object BasicMain extends App with Logging {
  val executor = Executors.newFixedThreadPool(100)
  val futurePool = FuturePool(executor)

  val config = SampleConfig()
  val connectionFactory = ConnectionFactory(config.hosts)

  // heartbeat を有効にします(default は無効)
  // 切断を検知するために必要です
  // 設定しないと、例えば channel を使った際などに、切断を検知するまで、数十秒待ちが発生します。
  // (java.net.SocketException が発生して復帰します)
  // 設定すれば、 設定値 * 2s くらいで切断を検知し、 shutdown します
  connectionFactory.setRequestedHeartbeat(3)

  val exchange = Exchange("", "")
  val queue = connectionFactory.withChannel { channel =>
  // 無名のキューを作成します
    Queue(channel.queueDeclare("", false, false, true, null).getQueue)
  }
  val routingKey = RoutingKey(queue.name)

  val producer = Producer(connectionFactory, exchange, routingKey)
  val consumer = Consumer(connectionFactory, queue)

  debug("start")

  futurePool(producer())
  futurePool(consumer())
}
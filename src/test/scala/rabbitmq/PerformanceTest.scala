package rabbitmq

/**
 * RabbitMQ のパフォーマンスを検証するサンプルです
 */

package study.rabbitmq

import com.twitter.util.{FuturePool}
import java.util.concurrent.{ExecutorService, TimeUnit, Executors}
import grizzled.slf4j.{Logging}
import com.rabbitmq.client.{QueueingConsumer, Connection, Channel}
import org.scalatest.FunSuite
import rabbitmq.SampleConfig
import Implicits._

trait ChannelHolder {
  def withChannel[A](f: Channel => A): A

  def close()
}

/**
 * channel を使いまわす人
 */
case class ChannelHolder1(factory: ConnectionFactory) extends ChannelHolder {
  val connection = factory.connect()
  val channel = connection.createChannel()
  channel.confirmSelect()

  def withChannel[A](f: Channel => A): A = {
    f(channel)
  }

  def close() {
    connection.close()
  }
}

/**
 * 毎回 channel を生成する人
 */
case class ChannelHolder2(factory: ConnectionFactory) extends ChannelHolder with Logging {
  val connection = factory.connect()

  def withChannel[A](f: Channel => A): A = {
    // Thread.currentThread().interrupt()
    // TODO このなかで interrupted フラグがクリアされるんです。。 しょぼぼーん
    val channel = connection.createChannel()
    channel.confirmSelect()

    try {
      f(channel)
    } finally {
      channel.close()
    }
  }

  def close() {
    trace("close start")
    connection.close()
    trace("close end")
  }
}

/**
 * connection を共有し、 channel は使いまわします
 */
case class ChannelHolder4(connection: Connection) extends ChannelHolder {
  val channel = connection.createChannel()
  channel.confirmSelect()

  def withChannel[A](f: Channel => A): A = {
    f(channel)
  }

  def close() {
  }
}

/**
 * ひたすら受ける人
 */
case class ConsumerLoop(channelHolder: ChannelHolder) extends ((Queue) => Unit) with Logging {
  var _receivedCount = 0

  def receivedCount: Int = _receivedCount

  def apply(queue: Queue) {
    try {
      trace("start " + queue)
      channelHolder.withChannel { channel =>
        val consumer = new QueueingConsumer(channel)
        channel.basicConsume(queue.name, consumer)
        while (true) {
          val delivery = consumer.nextDelivery()
          _receivedCount += 1
          channel.basicAck(delivery.tag, false)
        }
      }
    } finally {
      trace("finally")
      channelHolder.close()
    }
  }
}

trait Producer {
  def publish(exchange: Exchange, routingKey: RoutingKey, message: Array[Byte])

  def close()
}

/**
 * Ack 待ちをしないです
 */
case class NoAckProducer(channelHolder: ChannelHolder) extends Producer {
  def publish(exchange: Exchange, routingKey: RoutingKey, message: Array[Byte]) {
    channelHolder.withChannel { channel =>
      channel.basicPublish(exchange.name, routingKey.value, null, message)
    }
  }

  def close() {
    channelHolder.close()
  }
}

/**
 * Ack 待ちをするです
 */
case class AckProducer(channelHolder: ChannelHolder) extends Producer {
  def publish(exchange: Exchange, routingKey: RoutingKey, message: Array[Byte]) {
    channelHolder.withChannel { channel =>
      channel.basicPublish(exchange.name, routingKey.value, null, message)
      channel.waitForConfirms()
    }
  }

  def close() {
    channelHolder.close()
  }
}

/**
 * ひたすら入れる人
 */
case class ProducerLoop(producer: Producer) extends ((Exchange, RoutingKey) => Unit) with Logging {
  def apply(exchange: Exchange, routingKey: RoutingKey) {
    try {
      trace("producer start")

      for (i <- 0 to 100000) {
        try {
          val message = "test" + i
          producer.publish(exchange, routingKey, message.getBytes("UTF-8"))
          Thread.sleep(10) // interrupt 処理用
        } catch {
          case e: InterruptedException =>
            throw e
        }
      }
    } finally {
      trace("MyProducer finally")
      producer.close()
    }
  }
}

class PerformanceTest extends FunSuite with Logging {
  /**
   * consumer 負荷が低い状況では、 producer に依存しますね。
   *
   * max 2792/s くらいですね。
   */
  ignore("producer, consumer を増やすとどうなるか") {
    def create() = NoAckProducer(ChannelHolder1(factory))

    test1(1, create, 1) // 92/s
    test1(1, create, 3) // 92/s
    test1(3, create, 1) // 273/s
    test1(3, create, 3) // 276/s
    test1(10, create, 1) // 913/s
    test1(10, create, 10) // 934/s
    test1(20, create, 20) // 1660/s
    test1(30, create, 30) // 2324/s
    test1(40, create, 40) // 2858/s
    test1(50, create, 50) // 2792/s
  }

  ignore("限界はどの程度か") {
    def create() = NoAckProducer(ChannelHolder1(factory))

    test1(100, create, 100) // 959/s 明らか遅い
    test1(300, create, 1) // 946/s 明らか遅い
    test1(300, create, 2) // 2/s 明らか遅い
  }


  /**
   * あきらか遅いですね
   */
  ignore("channel を毎回作るのは遅いのか?") {
    // test1(1, () => ChannelHolder2(factory), 1) // 54/s
    // test1(10, () => ChannelHolder2(factory), 10) // 362/s
  }

  /**
   * 遅くない。 むしろ速い。 スレッドが増えてくると如実
   */
  ignore("connection をシェアすると遅いのか?") {
    val connection = factory.connect()
    def create() = NoAckProducer(ChannelHolder4(connection))
    try {
      test1(1, create, 1) // 92/s
      test1(50, create, 50) // 3045/s
      test1(300, create, 1) // 4896/s
      test1(500, create, 1) // 4216/s
    } finally {
      connection.close()
    }
  }

  /**
   * 結構変わりますね
   *
   * ack 待ち中に別の処理を行うべきか?
   * => そもそもマルチスレッドで同じ処理を並列に行うような実装であれば、あまり影響はないんじゃないかな。。と
   */
  test("ack 待ちをすると遅いのか") {
    val connection = factory.connect()
    def ack() = AckProducer(ChannelHolder4(connection))
    def noAck() = NoAckProducer(ChannelHolder4(connection))
    try {
      test1(1, ack, 1) // 66/s
      test1(1, noAck, 1) // 90/s
      test1(50, ack, 1) // 1704/s
      test1(50, noAck, 1) // 2534/s
      test1(100, ack, 1) // 2906/s
      test1(100, noAck, 1) // 2243/s
    } finally {
      connection.close()
    }
  }


  lazy val factory = ConnectionFactory(config.hosts)

  case class Stats(receivedCount: Int)

  /**
   * producers, consumers を起動し、実行後、性能を計測します
   */
  def test1(producerCount: Int, newProducer: () => Producer, consumerCount: Int) {
    debug("start")
    val executor = Executors.newFixedThreadPool(600)
    try {
      val futurePool = FuturePool(executor)

      val exchange = Exchange("", "")
      val queue = factory.withChannel { channel =>
        Queue(channel.queueDeclare("", false, false, true, null).getQueue)
      }
      val routingKey = RoutingKey(queue.name)

      val producers = (0 until producerCount).map(a => new ProducerLoop(newProducer()))
      val consumers = (0 until consumerCount).map(a => new ConsumerLoop(ChannelHolder2(factory)))

      for (producer <- producers) yield {
        futurePool {
          try {
            producer(exchange, routingKey)
          } catch {
            case e => trace(e)
          }
        }
      }

      for (consumer <- consumers) yield {
        futurePool {
          consumer(queue)
        }
      }

      debug("終了を待ちます")
      for (i <- 0 to 3) yield {
        Thread.sleep(1000)
        val receivedCount = consumers.map(_.receivedCount).sum
        val stats = Stats(receivedCount = receivedCount)
        debug("received: " + stats)
      }
    } finally {
      shutdown(executor)
    }
  }

  def config = SampleConfig()

  def shutdown(executor: ExecutorService) {
    trace("shutdown")

    executor.shutdown()
    executor.shutdownNow()
    if (!executor.awaitTermination(10, TimeUnit.SECONDS)) {
      debug("終了できませんでした。 しょぼーん")
    }
  }

  // channel.queueDeclare(queueName, false, false, false, Map("x-ha-policy" -> "all"))
}

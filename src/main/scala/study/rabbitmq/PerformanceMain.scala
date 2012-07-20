/**
 * RabbitMQ のパフォーマンスを検証するサンプルです
 */

package study.rabbitmq

import study.rabbitmq.Implicits._
import com.twitter.util.{FuturePool}
import java.util.concurrent.{ExecutorService, TimeUnit, Executors}
import grizzled.slf4j.{Logging}
import com.rabbitmq.client.{QueueingConsumer, Connection, Channel}

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
    Thread.currentThread().interrupt()
    // TODO このなかで interrupted フラグがクリアされるんです。。 しょぼぼーん
    val channel = connection.createChannel()
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

  def withChannel[A](f: Channel => A): A = {
    f(channel)
  }

  def close() {
  }
}

/**
 * ひたすら受ける人
 */
case class MyConsumer(channelHolder: ChannelHolder) extends ((Queue) => Unit) with Logging {
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

/**
 * ひたすら入れる人
 */
case class MyProducer(channelHolder: ChannelHolder) extends ((Exchange, RoutingKey) => Unit) with Logging {
  def apply(exchange: Exchange, routingKey: RoutingKey) {
    try {
      debug("producer start")

      for (i <- 0 to 100000) {
        try {
          val message = "test" + i
          channelHolder.withChannel { channel =>
            channel.basicPublish(exchange.name, routingKey.value, null, message.getBytes("UTF-8"))
          }
          Thread.sleep(10) // interrupt 処理用
        } catch {
          case e: InterruptedException =>
            debug("interrupted")
            throw e
        }
      }
    } finally {
      debug("MyProducer finally")
      channelHolder.close()
    }
  }
}

object PerformanceMain extends App with Logging {
  /**
   * producer, consumer を増やすとどうなるか
   *
   * consumer 負荷が低い状況では、 producer に依存しますね。
   *
   * max 2792/s くらいですね。
   */
  def test1() {
    test1(1, () => ChannelHolder1(factory), 1) // 92/s
    test1(1, () => ChannelHolder1(factory), 3) // 92/s
    test1(3, () => ChannelHolder1(factory), 1) // 273/s
    test1(3, () => ChannelHolder1(factory), 3) // 276/s
    test1(10, () => ChannelHolder1(factory), 1) // 913/s
    test1(10, () => ChannelHolder1(factory), 10) // 934/s
    test1(20, () => ChannelHolder1(factory), 20) // 1660/s
    test1(30, () => ChannelHolder1(factory), 30) // 2324/s
    test1(40, () => ChannelHolder1(factory), 40) // 2858/s
    test1(50, () => ChannelHolder1(factory), 50) // 2792/s
  }

  /**
   * channel を毎回作るのは遅いのか?
   *
   * あきらか遅いですね
   */
  def test2() {
    test1(1, () => ChannelHolder2(factory), 1) // 54/s
    test1(10, () => ChannelHolder2(factory), 10) // 362/s
  }

  /**
   * connection をシェアすると遅いのか?
   *
   * 遅くない。 むしろ速い?
   */
  def test3() {
    val connection = factory.connect()
    try {
      test1(1, () => ChannelHolder4(connection), 1) // 92/s
      test1(50, () => ChannelHolder4(connection), 50) // 3045/s
    } finally {
      connection.close()
    }
  }


  lazy val factory = ConnectionFactory(hosts)

  case class Stats(receivedCount: Int)

  /**
   * producers, consumers を起動し、実行後、性能を計測します
   */
  def test1(producerCount: Int, newProducerChannelHolder: () => ChannelHolder, consumerCount: Int) {
    val executor = Executors.newFixedThreadPool(100)
    try {
      val futurePool = FuturePool(executor)

      val exchange = Exchange("", "")
      val queue = withChannel { channel =>
        Queue(channel.queueDeclare("", false, false, true, null).getQueue)
      }
      val routingKey = RoutingKey(queue.name)

      val producers = (0 until producerCount).map(a => new MyProducer(newProducerChannelHolder()))
      val consumers = (0 until consumerCount).map(a => new MyConsumer(ChannelHolder2(factory)))

      for (producer <- producers) yield {
        futurePool {
          try {
            producer(exchange, routingKey)
          } catch {
            case e => debug(e)
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

  def hosts = args

  def shutdown(executor: ExecutorService) {
    trace("shutdown")

    executor.shutdown()
    executor.shutdownNow()
    if (!executor.awaitTermination(10, TimeUnit.SECONDS)) {
      debug("終了できませんでした。 しょぼーん")
    }
  }

  def withChannel[A](f: Channel => A): A = {
    val channelHolder = ChannelHolder2(factory)
    try {
      channelHolder.withChannel(f)
    } finally {
      channelHolder.close()
    }
  }


  // channel.queueDeclare(queueName, false, false, false, Map("x-ha-policy" -> "all"))
}
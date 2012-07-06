package study.rabbitmq
import com.rabbitmq.client.ConnectionFactory
import com.rabbitmq.client.Connection
import scala.actors.Actor
import com.rabbitmq.client.Address
import com.rabbitmq.client.DefaultConsumer
import com.rabbitmq.client.Envelope
import com.rabbitmq.client.AMQP
import com.rabbitmq.client.Channel
import java.net.ConnectException
import scala.collection.JavaConversions._
import com.rabbitmq.client.AlreadyClosedException
import java.util.HashMap

class Exchange(val name: String, val exchangeType: String) {

}

object RabbitMQ {
  lazy val connectionFactory = {
    val factory = new ConnectionFactory
    factory
  }
}

/**
 * MQ との通信経路を表す
 *
 * - ロードバランスを行う?
 */
class Session(hosts: Array[String]) {
  var connection = connect()
  var channel = connection.createChannel()

  /**
   * MQ に対して処理を実行する
   * - 通信失敗時は、他の経路にて処理を再施行する
   *
   * 最終処理以外は何度実行しても問題ないように実装すること
   */
  def execute(f: (Channel) => Unit) {
    try {
      f(channel)
    } catch {
      // TODO 1つにまとめられないのかな？
      case e: ConnectException =>
        println("リトライ")
        connection = connect()
        channel = connection.createChannel()
        execute(f)
      case e: AlreadyClosedException =>
        println("リトライ")
        connection = connect()
        channel = connection.createChannel()
        execute(f)
    }
  }

  def close() {
    if (channel.isOpen) {
      channel.close()
    }
    if (connection.isOpen) {
      connection.close()
    }
  }

  private def connect() = {
    RabbitMQ.connectionFactory.newConnection(hosts.map(new Address(_)))
  }
}

/**
 * ひたすら入れる人
 */
class Producer(hosts: Array[String], name: String, start: Int, exchange: Exchange) extends Actor {
  def act {
    Actor.self ! ("send", start)
    loop {
      react {
        case ("send", i: Int) =>
          session.execute { channel =>
            val message = "mes" + (start + i)
            channel.basicPublish(exchange.name, "route_mail", null, message.getBytes())
            println("%s sent '%s'".format(name, message))
          }
          sleep(i + 1)
      }
    }
  }

  val session = new Session(hosts)
  session.execute { channel =>
    channel.exchangeDeclare(exchange.name, exchange.exchangeType)
  }

  def sleep(i: Int) = {
    val main = Actor.self
    Actor.actor {
      // Thread.sleep(1000)
      main ! ("send", i)
    }
  }
}

/**
 * ひたすら受ける人
 */
class MyConsumer(hosts: Array[String], queueName: String) {
  def run() = {
    println("consumer start")
    val session = new Session(hosts)
    // TODO これじゃ再試行できない

    session.execute { channel =>
      val consumer = new DefaultConsumer(channel)
      channel.basicConsume(queueName, new DefaultConsumer(channel) {
        override def handleDelivery(consumerTag: String,
          envelope: Envelope,
          properties: AMQP.BasicProperties,
          body: Array[Byte]) {

          val message = new String(body, "UTF-8")

          val deliveryTag = envelope.getDeliveryTag
          println("received '" + message + "'")
          channel.basicAck(deliveryTag, false)
        }
      })
    }
  }
}

object RabbitMQMain {
  def main(args: Array[String]) {
    val hosts = Array("dev-mq1", "dev-mq2", "dev-mq3")
    val exchange = new Exchange("direct", "direct")
    val queueName = "queue2"

    val session = new Session(hosts)
    session.execute { channel =>
      channel.exchangeDeclare(exchange.name, exchange.exchangeType)
      channel.queueDeclare(queueName, false, false, false, Map("x-ha-policy" -> "all"))
      channel.queueBind(queueName, exchange.name, "route_mail")
    }

    val producers = for (i <- 1 to 1) yield {
      new Producer(hosts, "p" + i, i * 1000, exchange)
    }

    val consumers = for (i <- 1 to 10) yield {
      new MyConsumer(hosts, queueName)
    }
    // producers.foreach(_.start)
    consumers.foreach(_.run)
  }
}
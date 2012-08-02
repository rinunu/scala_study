/**
 * RabbitMQ を使用する際のヘルパーです。
 */

package rabbitmq

import com.rabbitmq.client
import com.rabbitmq.client.{Address, Connection, Channel}
import com.rabbitmq.client.QueueingConsumer.Delivery


case class Exchange(name: String, exchangeType: String)

case class RoutingKey(value: String)

case class Queue(name: String) {
  require(name != null)
}

case class ConnectionFactory(hosts: Seq[String]) {
  val impl = new client.ConnectionFactory

  def connect(): Connection = {
    impl.newConnection(hosts.map(new Address(_)).toArray)
  }

  def close() {
  }

  val setRequestedHeartbeat = impl.setRequestedHeartbeat _

  /**
   * 一時的な Channel を作成し、処理を行います
   */
  def withChannel[A](f: Channel => A): A = {
    val connection = connect()
    try {
      val channel = connection.createChannel()
      f(channel)
    } finally {
      connection.close()
    }
  }
}

class RichDelivery(impl: Delivery) {
  def string: String = new String(impl.getBody, "UTF-8")

  val tag: Long = impl.getEnvelope.getDeliveryTag
}

object Implicits {
  implicit def toDelivery(a: Delivery): RichDelivery =
    new RichDelivery(a)
}
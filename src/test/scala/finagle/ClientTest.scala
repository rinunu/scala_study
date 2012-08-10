/**
 * Finagle クライアント側のサンプルです
 *
 * https://github.com/twitter/finagle/blob/master/README.md#Top
 *
 *
 */
package finagle

import org.scalatest.FunSuite
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.Http
import java.net.InetSocketAddress
import org.jboss.netty.util.CharsetUtil
import org.jboss.netty.handler.codec.http._
import com.twitter.finagle.{SimpleFilter, Service}
import com.twitter.util.Future
import grizzled.slf4j.Logging

class ClientTest extends FunSuite with Logging {
  /**
   * Service を作ります
   *
   * 構築される Service は詳細を省略すると、こんな感じになります
   *
   * {{{
   * val hostFactories = cluster.map {
   *   TimeoutFactory andThen
   *   WatermarkPool andThen
   *   ChannelServiceFactory
   * }
   *
   * var factory = TimeoutFactory(
   *   HeapBalancer(hostFactories))
   *
   * var service = FactoryToService(factory)
   *
   * val result = TimeoutFilter antThen
   *   RetryingFilter andThen
   *   service
   *
   * }}}
   */
  def newService(addr: InetSocketAddress): Service[HttpRequest, HttpResponse] = ClientBuilder()
    .codec(Http())
    .hosts(addr) // cluster を設定します
    .hostConnectionLimit(1)
    .build()

  /**
   * service を使用し、その後 release します
   */
  def withService[A](service: Service[_, _])(f: => A): A = {
    try {
      f
    } finally {
      service.release()
    }
  }

  test("get") {
    val service = newService(new InetSocketAddress("www.google.com", 80))
    withService(service) {
      val req = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/")

      val resFuture = service(req)
      val res = resFuture() // 同期的に結果を取得する

      val str = res.getContent.toString(CharsetUtil.UTF_8)
      debug((res.getStatus, res.getHeaders, str))
    }
  }

  test("filter - service の処理の前後に別の処理をいれる") {
    case class MyFilter(name: String) extends SimpleFilter[HttpRequest, HttpResponse] {
      def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] = {
        debug(name + " start")
        val res = service(request)
        debug(name + "end")
        res
      }
    }

    val service = newService(new InetSocketAddress("www.google.com", 80))
    // filter でくるむ
    val service1 = MyFilter("filter0") andThen MyFilter("filter1") andThen service

    withService(service1) {
      val req = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/")

      val res = service1(req)()

      val str = res.getContent.toString(CharsetUtil.UTF_8)
      debug((res.getStatus, res.getHeaders, str))
    }
  }
}


/**
 * Service
 * - 使うたびに factory から生成し、 release する前提だと思います(FactoryToService がそうなっているので)
 * - この時、実際には CachingPool を挟んでパフォーマンスを上げるようです。
 * - ただし、ルートとなる Service はずっと開きっぱなしと思われます。 なぜなら  factory が無いためです。
 *
 * ServiceFactory
 *
 * FactoryToService
 * - service を使用するたびに、 factory.apply, service.apply, service.release します。
 *
 * HeapBalancer
 * - apply すると service を取得できます。 このタイミングでバランシングされます。
 * - つまり、頻繁に apply しないとバランシングしないと思われますが、頻繁に apply されるはずです。
 * - なぜなら FactoryToService がそうなっているためです。
 *
 */
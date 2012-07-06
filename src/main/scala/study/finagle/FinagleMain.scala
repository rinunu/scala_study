package study.finagle
import com.twitter.finagle.Service
import org.jboss.netty.handler.codec.http.HttpRequest
import org.jboss.netty.handler.codec.http.HttpResponse
import com.twitter.util.Future
import java.net.InetSocketAddress
import java.net.SocketAddress
import com.twitter.finagle.builder.Server
import com.twitter.finagle.builder.ServerBuilder
import org.jboss.netty.handler.codec.http.DefaultHttpResponse

import com.twitter.finagle.{ Service, SimpleFilter }
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.codec.http.HttpResponseStatus._
import org.jboss.netty.handler.codec.http.HttpVersion.HTTP_1_1
import org.jboss.netty.buffer.ChannelBuffers.copiedBuffer
import org.jboss.netty.util.CharsetUtil.UTF_8
import com.twitter.util.Future
import java.net.InetSocketAddress
import com.twitter.finagle.builder.{ Server, ServerBuilder }
import com.twitter.finagle.http.Http

object FinagleMain {
  def main(args: Array[String]) {
    val service: Service[HttpRequest, HttpResponse] = new Service[HttpRequest, HttpResponse] {
      def apply(request: HttpRequest) = Future(new DefaultHttpResponse(HTTP_1_1, OK))
    }

    val address: SocketAddress = new InetSocketAddress(10000)

    val server = ServerBuilder()
      .codec(Http())
      .bindTo(address)
      .name("HttpServer")
      .build(service)

  }
}
package twitter_future

import java.util.concurrent.{TimeUnit, Executors}
import com.twitter.util._
import collection.mutable.ListBuffer
import com.twitter.conversions.time._

trait Common {

  class MyFixture {
    val executorService = Executors.newFixedThreadPool(10)
    val futurePool = FuturePool(executorService)

    def ok = futurePool {
      "ok"
    }

    /**
     * 指定秒数寝て、ログを吐く
     */
    def log(sleepTime: Duration, s: String): Future[String] = futurePool {
      sleep(sleepTime)
      logs += s
      s
    }

    val ng: Future[String] = futurePool {
      throw new RuntimeException
    }

    val logs = ListBuffer[String]()

    def awaitTermination() {
      executorService.awaitTermination(10000, TimeUnit.MILLISECONDS)
    }
  }

  val sleep: Long => Unit = Thread.sleep
}

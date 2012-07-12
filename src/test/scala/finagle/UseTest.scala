package finagle

import org.scalatest.FunSuite
import com.twitter.util.{FuturePool, Promise}
import java.util.concurrent.Executors
import collection.mutable.ListBuffer

class UseTest extends FunSuite {

  class MyFixture {
    val executorService = Executors.newFixedThreadPool(10)
    val futurePool = FuturePool(executorService)

    val success0 = futurePool {
      "ok"
    }

    val success1 = futurePool {
      "ok"
    }

    val failure0 = futurePool {
      throw new RuntimeException
    }

    val logs = ListBuffer[String]()
  }

  test("コールバック") {
    val f = new MyFixture
    // chained したものは順番に実行される
    f.failure0.onFailure {
      case e => f.logs += "a"
    }.onFailure {
      case e => f.logs += "b"
    }
    // 並列につけたものの順序はよくわからない
    f.failure0.onFailure {
      case e => f.logs += "c"
    }

    assert(f.logs === Seq("a", "b", "c"))
  }

  test("onFailure - 失敗した際のコールバック") {
    val f = new MyFixture

    f.failure0.onFailure {
      case e => f.logs += "a"
    }

    assert(f.logs === Seq("a"))
  }

  ignore("コールバック内での失敗") {
    val f = new MyFixture

    // これはだめ。例外がスレッドプールのルートまでいっちゃう
    f.success0.onSuccess { a =>
      f.logs += "a"
      throw new RuntimeException
    }.onFailure {
      case e => f.logs += "b"
    }
    // ensure も一緒
  }
}

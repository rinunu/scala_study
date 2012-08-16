package twitter_future

import org.scalatest.FunSuite
import com.twitter.util.{FuturePool, Promise}
import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer

class ChainTest extends FunSuite with Common {

  test("コールバック") {
    val f = new MyFixture
    // chained したものは順番に実行される
    f.ng.onFailure {
      case e => f.logs += "a"
    }.onFailure {
      case e => f.logs += "b"
    }
    // 並列につけたものの順序はよくわからない
    f.ng.onFailure {
      case e => f.logs += "c"
    }

    assert(f.logs === Seq("a", "b", "c"))
  }

}

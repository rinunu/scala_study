package twitter_future

import com.twitter.util._
import org.scalatest.FunSuite
import java.util.concurrent.{RejectedExecutionException, TimeUnit, Executors}
import com.twitter.util.Throw
import com.twitter.util.Return
import com.twitter.conversions.time._
import scala.util.control.Exception._

/**
 * 何もない状態から future を作る方法
 */
class CreateTest extends FunSuite with Common {

  test("同期処理をそのまま Future でくるむ") {
    val f = Future {
      "a"
    }
    assert(f.isDefined)
    assert(f() === "a")
  }

  test("非同期処理用の Future を作る(自前非同期) - 成功") {
    // 自前で別スレッドを立てる前提です。

    val fu = new Promise[String]() // Future サブクラス
    assert(!fu.isDefined)

    // 値をセットする
    fu() = Return("a")
    assert(fu.isDefined)
    assert(fu.isDefined)
    assert(fu() === "a")
  }

  test("非同期処理用の Future を作る(自前非同期) - 失敗") {
    // 自前で別スレッドを立てる前提です。

    val fu = new Promise[String]() // Future サブクラス

    // 失敗を future にセットする
    fu() = Throw(new RuntimeException(""))
    intercept[RuntimeException] {
      fu()
    }
  }

  test("非同期処理用の Future を作る(FuturePool) - 成功") {
    val f = new MyFixture
    val futurePool = FuturePool(f.executorService)

    // これで非同期処理が開始し、その future が手に入る
    val fu = futurePool {
      sleep(100)
      "a"
    }
    assert(!fu.isDefined, "まだ結果は設定されていない")
    assert(fu() === "a")
  }

  test("非同期処理用の Future を作る(FuturePool) - 失敗") {
    val f = new MyFixture
    val futurePool = FuturePool(f.executorService)

    // 例外を投げると future に例外がセットされる
    val fu = futurePool {
      throw new RuntimeException()
    }
    intercept[RuntimeException] {
      fu()
    }
  }

  test("FuturePool : shutdown") {
    val f = new MyFixture
    val futurePool = FuturePool(f.executorService)
    f.executorService.shutdown()

    def fu(i: Int) = futurePool {
      f.logs += i.toString
      i
    }

    val fu0 = fu(0).ensure {
      f.logs += "ensure" // 呼ばれる
    }

    intercept[RejectedExecutionException] {
      fu0()
    }
    assert(f.logs == Seq("ensure"))
  }

  test("whileDo : 無限ループ + 例外処理") {
    val f = new MyFixture

    var i = 0
    Future.whileDo(true) {
      f.futurePool {
        sleep(10.millis)
        f.logs += i.toString
        i += 1
        if (i == 2) {
          throw new RuntimeException
        }
      }.rescue {
        // shutdown 以降は RejectedExecutionException が発生する。 これは無視したい。
        case e if !shouldRethrow(e) && !e.isInstanceOf[RejectedExecutionException] =>
          f.logs += "e"
          Future("a")
      }
    }

    sleep(50)
    f.executorService.shutdown()
    assert(f.executorService.awaitTermination(1000, TimeUnit.MILLISECONDS), "ちゃんと終了すること")
    assert(f.logs === Seq("0", "1", "e", "2", "3", "4"))
  }

  test("Future.monitored") {

  }

  test("Future#linkto") {

  }
}

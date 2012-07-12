package finagle

import com.twitter.util._
import org.scalatest.FunSuite
import java.util.concurrent.Executors
import com.twitter.util.Throw
import com.twitter.util.Return
import Thread.sleep

/**
 * future の作り方
 */
class FinagleFutureTest extends FunSuite {

  class MyFixture {
    val executorService = Executors.newFixedThreadPool(10)
  }

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

  test("Future.monitored") {

  }

  test("Future#linkto") {

  }
}

package twitter_future

import org.scalatest.FunSuite
import com.twitter.util.{Future, FuturePool}
import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer

/**
 * 失敗の扱い方
 */
class FailureTest extends FunSuite with Common {
  test("onFailure : 失敗した際のコールバック") {
    val f = new MyFixture

    f.ng.onFailure {
      case e => f.logs += "a"
    }

    assert(f.logs === Seq("a"))
  }

  test("onFailure : 指定しなかった例外が発生した場合 => ") {
    val f = new MyFixture

    // MatchError がワーカースレッドのルートまで到達してしまい、エラーとなる。 ので、やっちゃだめかな。。
    val f0 = Future(throw new IllegalStateException).onFailure {
      case e: IllegalArgumentException => f.logs += "a"
    }
  }

  ignore("コールバック内での失敗は失敗") {
    val f = new MyFixture

    // これはだめです。例外がスレッドプールのルートまでいっちゃうみたいです
    f.ok.onSuccess {
      a =>
        f.logs += "a"
        throw new RuntimeException
    }.onFailure {
      case e => f.logs += "b"
    }
    // ensure も一緒
  }

  test("rescue 失敗をリカバリし、成功にする") {
    val f = new MyFixture

    val fu0 = f.ng

    // fu0 と fu1 は挙動が異なります。
    // 実際には rescue とかよんだ場合は、結果の fu1 を使うべきだと思います。
    val fu1 = fu0.onFailure {
      case e =>
        f.logs += "a" // 呼ばれる
    }.rescue {
      case e =>
        f.logs += "b"
        f.ok
    }.onSuccess { s =>
      f.logs += "c" // 呼ばれる
    }.onFailure {
      case e =>
        f.logs += "d" // 呼ばれない
    }

    intercept[RuntimeException] {
      fu0()
    }

    assert(fu1() === "ok")
    assert(f.logs === Seq("a", "b", "c"))
  }

  test("ensure : 成功時 / 失敗時に処理を行う") {
    val f = new MyFixture

    val fu0 = f.ok.ensure {
      f.logs += "a"
    }

    // リカバリするわけではない。 finally みたいな立ち位置
    val fu1 = f.ng.ensure {
      f.logs += "b"
    }

    fu0()
    intercept[RuntimeException] {
      fu1()
    }
    assert(f.logs === Seq("a", "b"))
  }

}

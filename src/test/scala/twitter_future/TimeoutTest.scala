package twitter_future

import org.scalatest.FunSuite
import com.twitter.conversions.time._
import com.twitter.util.{JavaTimer, TimeoutException}

class TimeoutTest extends FunSuite with Common {
  test("apply のタイムアウト") {
    val f = new MyFixture
    val fu = f.futurePool {
      Thread.sleep(10)
      f.logs += "a"
      "a"
    }

    intercept[TimeoutException] {
      fu(1.millis) // 実装メモ: iver にてポーリング
    }

    // タイムアウトしたからといって失敗するわけではないので、もう一度待てます。
    // 逆に言うと、タイムアウトした後も処理は継続しているわけなので、
    // 処理をやめるのであれば、明示的に cancel しないといけません。

    Thread.sleep(20.millis)
    assert(f.logs == Seq("a"))

    assert(fu() === "a")
  }

  test("within : タイムアウトすると失敗する future を作ります") {
    val f = new MyFixture
    val fu0 = f.log(20.millis, "a")

    // タイムアウト時に fu0 の キャンセルはしません
    val fu1 = fu0.within(new JavaTimer, 10.millis).ensure {
      // なので、こことかで後始末をしないと駄目かもです
      f.logs += "b"
    }

    intercept[TimeoutException] {
      fu1()
    }
    assert(f.logs === Seq("b"))

    // キャンセルされていないので
    sleep(30)
    assert(f.logs === Seq("b", "a"))
  }

  test("apply, within 両方組み合わせ") {
    val f = new MyFixture
    val fu0 = f.log(40.millis, "a")

    val fu1 = fu0.within(new JavaTimer, 20.millis).ensure {
      f.logs += "b"
    }

    intercept[TimeoutException] {
      fu1(10.millis) // このタイムアウトが発生する
    }
    assert(f.logs === Seq())
    sleep(15.millis)
    assert(f.logs === Seq("b")) // そして within のタイムアウトが発生する
    sleep(30.millis)
    assert(f.logs === Seq("b", "a")) // fu0 もキャンセルされていないので
  }
}

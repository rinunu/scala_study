package study.collection

import org.scalatest.FunSuite

/**
 * 遅延評価する List
 *
 */
class StreamTest extends FunSuite {

  case class LargeObject(n: Int) {
    val large = new Array[Int](1000 * 1000)
  }

  def hugeStream(start: Int = 0): Stream[LargeObject] = {
    LargeObject(start) #:: hugeStream(start + 1)
  }

  test("foldLeft してもメモリを消費しない") {
    // val stream = ただし、 stream への参照を保持しちゃうとダメ。

    val sum = hugeStream().take(1000).foldLeft(0) { (z, a) =>
      Thread.sleep(10)
      println(a.n)
      z + a.n
    }
    println(sum)
  }
}

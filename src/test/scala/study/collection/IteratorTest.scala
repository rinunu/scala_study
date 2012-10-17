package collection

import org.scalatest.FunSuite

class IteratorTest extends FunSuite {
  test("iterate: 無限 iterator を作ります") {
    val it = Iterator.iterate(1)(_ + 1)
    assert(it.take(3).toSeq === Seq(1, 2, 3))
  }

  test("take - 先頭のいくつかの要素を取り出す") {
    val it = Iterator.from(0)
    assert(it.take(3).toList === List(0, 1, 2))
    assert(it.take(3).toList === List(3, 4, 5))
  }

  test("takeWhile - 先頭の幾つかの要素を取り出す") {
    val it = Iterator.from(0)
    val it2 = it.takeWhile(_ < 3)
    assert(it2.toList === List(0, 1, 2)) // このタイミングで it も進む
    assert(it.take(3).toList === List(4, 5, 6)) // 3 は読み飛ばされている. it にアクセスするのはあまり良くない気がする
  }

  test("buffered") {
    val it = Iterator.from(0).buffered
    val it2 = it.takeWhile(_ < 3)
    assert(it2.toList === List(0, 1, 2)) // このタイミングで it も進む. 3 まで読み込む
    assert(it.head === 4)
  }

  test("filter, map") {
    val it = Iterator.from(0)
    val it2 = it.filter(_ != 3).map(_ * 2) // ここでは実行しません
    assert(it2.take(5).toList === List(0, 2, 4, 8, 10))
  }
}

package collection

import org.scalatest.FunSuite

class IteratorTest extends FunSuite {
  test("iterate: 無限 iterator を作ります") {
    val it = Iterator.iterate(1)(_ + 1)
    assert(it.take(3).toSeq === Seq(1, 2, 3))
  }
}

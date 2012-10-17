package algorithm

import org.scalatest.FunSuite

class Stack[A](private var top: Int, a: Array[A]) {
  def empty: Boolean = top == 0

  def push(x: A) {
    top += 1
    a(top) = x
  }

  def pop(): A = {
    if (empty) {
      sys.error("アンダーフロー")
    } else {
      top -= 1
      a(top + 1)
    }
  }
}

object Stack {
  def apply[A](top: Int, a: Array[A]): Stack[A] = new Stack(top, a)

  def apply[A: Manifest](top: Int, n: Int): Stack[A] = new Stack(top, new Array[A](n))
}

class StackTest extends FunSuite {

  test("empty") {
    assert(Stack[Int](0, 10).empty, "空のもの")
    assert(!Stack[Int](1, 10).empty, "空じゃないよ")
  }

  test("push/pop") {
    val s = Stack[Int](0, 10)
    s.push(1)
    assert(!s.empty, "入れると空じゃないよ")

    val x = s.pop()
    assert(x === 1)
  }

  test("pop - アンダーフロー") {
    val s = Stack[Int](0, 10)

    intercept[Exception] {
      val x = s.pop()
    }
  }
}

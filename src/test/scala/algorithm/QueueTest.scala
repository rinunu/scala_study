package algorithm

import org.scalatest.FunSuite

class Queue[A](a: Array[A]) {
  private var tail = 0
  private var head = a.size

  def enqueue(x: A) {

  }

  def dequeue(): A = {
    sys.error("TODO")
  }
}

object Queue {
  def apply[A: Manifest]() = new Queue[A](new Array[A](10))
}

class QueueTest extends FunSuite {

  test("enqueue/dequeue") {
    val q = Queue[Int]()
    q.enqueue(1)
    assert(q.dequeue() === 1)
  }

}

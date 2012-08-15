package algorithm

import org.scalatest.FunSuite

class HeapTest extends FunSuite {

  import Heap._

  test("parent") {
    assert(parent(3) === 1)
    assert(parent(4) === 1)
    assert(parent(5) === 2)
    assert(parent(6) === 2)
  }

  test("left") {
    assert(left(0) === 1)
    assert(left(1) === 3)
    assert(left(5) === 11)
    assert(left(6) === 13)
  }

  test("right") {
    assert(right(0) === 2)
    assert(right(1) === 4)
    assert(right(5) === 12)
    assert(right(6) === 14)
  }

  test("maxHeapify: (16,4,10), 0") {
    assert(maxHeapify(Heap(16, 4, 10), 0).a === Array(16, 4, 10))
  }

  test("maxHeapify: (4, 16, 10), 1") {
    assert(maxHeapify(Heap(4, 16, 10), 0).a === Array(16, 4, 10))
  }

  test("maxHeapify: long") {
    assert(maxHeapify(Heap(16, 4, 10, 14, 7, 9, 3, 2, 8, 1), 1).a === Array(16, 14, 10, 8, 7, 9, 3, 2, 4, 1))
  }

  test("buildMaxHeap: long") {
    assert(buildMaxHeap(Heap(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)).a ===
      Array(16, 14, 10, 8, 7, 9, 3, 2, 4, 1))
  }

  test("minHeapify: (16,4,10), 0") {
    assert(minHeapify(Heap(16, 4, 10), 0).a === Array(4, 16, 10))
  }

  test("buildMinHeap: long") {
    assert(buildMinHeap(Heap(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)).a ===
      Array(1, 2, 3, 4, 7, 9, 10, 14, 8, 16))
  }

}

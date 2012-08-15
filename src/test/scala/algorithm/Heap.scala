package algorithm

case class Heap(a: Array[Int], heapSize: Int)

object Heap {

  import Sort.swap

  def apply(values: Int*): Heap = Heap(values.toArray, values.length)

  def apply(values: Array[Int]): Heap = Heap(values, values.length)

  def parent(i: Int): Int = (i + 1) / 2 - 1

  def left(i: Int): Int = (i + 1) * 2 - 1

  def right(i: Int): Int = left(i) + 1

  def maxHeapify(a: Heap, i: Int): Heap = {
    val l = left(i)
    val r = right(i)

    val _largest = if (l < a.heapSize && a.a(l) > a.a(i)) l else i
    val largest = if (r < a.heapSize && a.a(r) > a.a(_largest)) r else _largest

    if (largest != i) {
      swap(a.a, largest, i)
      maxHeapify(a, largest)
    } else {
      a
    }
  }

  def minHeapify(a: Heap, i: Int): Heap = {
    val l = left(i)
    val r = right(i)

    val _smallest = if (l < a.heapSize && a.a(l) < a.a(i)) l else i
    val smallest = if (r < a.heapSize && a.a(r) < a.a(_smallest)) r else _smallest

    if (smallest != i) {
      swap(a.a, smallest, i)
      minHeapify(a, smallest)
    } else {
      a
    }
  }

  def buildMaxHeap(a: Heap): Heap = {
    // TODO ちょっと無駄が?
    for (i <- ((a.a.size + 1) / 2) to 0 by -1) {
      maxHeapify(a, i)
    }
    a
  }

  def buildMinHeap(a: Heap): Heap = {
    // TODO ちょっと無駄が?
    for (i <- ((a.a.size + 1) / 2) to 0 by -1) {
      minHeapify(a, i)
    }
    a
  }

}


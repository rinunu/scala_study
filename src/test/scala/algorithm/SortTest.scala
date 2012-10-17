package algorithm

import org.scalatest.FunSuite
import scala.collection.mutable.ArrayBuffer
import annotation.tailrec

object MyList {

  class Node[A](val v: A, var next: Node[A] = null)

  def append[A](head: Node[A], n: Node[A]): Node[A] = {
    if (head.next == null)
      head.next = n
    else
      append(head.next, n)
    head
  }

  def size(head: Node[_]): Int =
    if (head.next == null) 1
    else size(head.next) + 1

  def copyTo[A](head: Node[A], a: Array[A], i: Int = 0): Array[A] = {
    a(i) = head.v
    if (head.next != null) {
      copyTo(head.next, a, i + 1)
    }
    a
  }
}

object Sort {
  /**
   * 0 <= a(n) < 1
   */
  def bucketSort(a: Array[Double]): Array[Double] = {
    import MyList._
    def getBucketNo(v: Double) = (v * 10).toInt

    // [0, n) のバケツに、a を入れていきます
    val n = 10
    val b = new Array[Node[Double]](n)
    for (v <- a) {
      val bucketNo = getBucketNo(v)
      log("bucketNo", bucketNo)
      val node = new Node(v)
      b(bucketNo) = if (b(bucketNo) == null) node
      else append(b(bucketNo), node)
    }

    val notEmptyList = b.filter(_ != null)

    for(n <- notEmptyList){
      // TODO
    }

    val result = new Array[Double](a.size)
    notEmptyList.foldLeft(0) { (i, node) =>
      copyTo(node, result, i)
      i + size(node)
    }
    result
  }

  def insertionSort[A <% Ordered[A]](seq: Array[A]): Array[A] = {
    for (i <- 0 until seq.size) {
      val key = seq(i)
      // key をソート済みの列 seq(0..i) に挿入する
      for (j <- (i - 1) to 0 by -1)
        if (key < seq(j)) {
          seq(j + 1) = seq(j)
          seq(j) = key
        }
    }
    seq
  }

  /**
   * functional style だとこんな風に書くそうな。
   * http://stackoverflow.com/questions/1672074/selection-sort-in-functional-scala
   */
  def selectionSort(seq: Array[Int]): Array[Int] = {
    def findMin(start: Int) = {
      var minIndex = start
      var min = seq(start)
      var j = start
      while (j < seq.size) {
        if (seq(j) < min) {
          minIndex = j
          min = seq(j)
        }
        j += 1
      }
      (minIndex, min)
    }

    for (i <- 0 until seq.size) {
      var (minIndex, min) = findMin(i)
      seq(minIndex) = seq(i)
      seq(i) = min
    }
    seq
  }

  def mergeSort(seq: Array[Int]): Array[Int] = {
    def merge(seq0: Array[Int], seq1: Array[Int]): Array[Int] = {
      log("mergeするよ", seq0.toSeq, seq1.toSeq)
      var i0, i1 = 0
      var resultSeq = ArrayBuffer[Int]()
      while (i0 != seq0.size || i1 != seq1.size) {
        log((i0, seq0.size), (i1, seq1.size))
        if (i0 == seq0.size) {
          resultSeq += seq1(i1)
          i1 += 1
        } else if (i1 == seq1.size) {
          resultSeq += seq0(i0)
          i0 += 1
        } else if (seq0(i0) < seq1(i1)) {
          resultSeq += seq0(i0)
          i0 += 1
        } else {
          resultSeq += seq1(i1)
          i1 += 1
        }
      }
      log("mergeしたよ", resultSeq)
      resultSeq.toArray
    }

    seq.size match {
      // case 0 はないです
      case 1 => seq
      case _ =>
        val (sub0, sub1) = seq.splitAt(seq.size / 2)
        log("分割したよ", sub0.toSeq, sub1.toSeq)
        merge(mergeSort(sub0), mergeSort(sub1))
    }
  }

  def bubbleSort(seq: Array[Int]): Array[Int] = {
    for (unsortedEnd <- seq.size to 1 by -1)
      for (i <- 1 until unsortedEnd) {
        if (seq(i - 1) > seq(i)) {
          swap(seq, i - 1, i)
        }
      }
    seq
  }

  def heapSort(seq: Array[Int]): Array[Int] = {
    import Heap._
    val a = buildMaxHeap(Heap(seq))
    val b = ((a.a.size - 1) to 1 by -1).foldLeft(a) { (a, i) =>
      swap(a.a, 0, i)
      maxHeapify(a.copy(heapSize = a.heapSize - 1), 0)
    }
    b.a
  }

  // qsort []     = []

  // qsort (p:xs) = qsort lt ++ [p] ++ qsort gteq
  //where
  // lt   = [x | x <- xs, x < p]
  // gteq = [x | x <- xs, x >= p]

  def quickSort2(a: Array[Int]): Array[Int] = {
    def qsort(a: List[Int]): List[Int] = {
      a match {
        case Nil => Nil
        case p :: xs =>
          val lt = for (x <- xs if x < p) yield x
          val gteq = for (x <- xs if x >= p) yield x
          qsort(lt) ++ List(p) ++ qsort(gteq)
      }
    }
    qsort(a.toList).toArray
  }

  def quickSort(a: Array[Int]): Array[Int] = {
    // a(p, r) を分割します
    // 小さいものと大きいものに分割します
    // pivot の位置を返します
    def partition(p: Int, r: Int): Int = {
      val x = a(r)
      // i は x より小さい値の範囲の末尾を指します
      var i = p - 1
      // j は i に先行し、 x より小さいものを見つけると i と値を交換します
      for (j <- p to r - 1) {
        if (a(j) <= x) {
          i += 1
          swap(a, i, j)
        }
      }
      // a(i + 1) は x より大きい
      swap(a, i + 1, r)
      log(i + 1, a.toSeq)
      i + 1
    }


    def quickSortImpl(p: Int, r: Int) {
      log(p, r)
      if (p < r) {
        val q = partition(p, r)
        quickSortImpl(p, q - 1)
        quickSortImpl(p + 1, r)
      }
    }

    quickSortImpl(0, a.size - 1)
    a
  }

  def countingSort(a: Array[Int]): Array[Int] = {
    log("start!", a.toSeq)
    // c[v] が、 値 v に対する情報を持ちます。
    val c = new Array[Int](100) // サイズは適当にね.

    // 値 v の数を数えます
    for (i <- 0 until a.size) {
      c(a(i)) += 1
    }
    log(c.toSeq)

    // 各値 v より小さい値の数を数えます
    for (i <- 1 until c.size) {
      c(i) += c(i - 1)
    }
    log(c.toSeq)

    // 値 v は、 c(v) - 1 の位置に行くといいです
    val b = new Array[Int](a.size)
    for (i <- a.size - 1 to 0 by -1) {
      val v = a(i)
      b(c(v) - 1) = v
      c(v) -= 1
    }
    b
  }

  def swap(seq: Array[Int], i: Int, j: Int) {
    val tmp = seq(i)
    seq(i) = seq(j)
    seq(j) = tmp
  }

  private def log(s: Object) {
    println(s)
    Thread.sleep(10)
  }
}

trait SortTest extends FunSuite {
  val sort: Array[Int] => Array[Int]

  test("3") {
    assert(sort(Array(3)) === Array(3))
  }

  test("23") {
    assert(sort(Array(2, 3)) === Array(2, 3))
  }

  test("32") {
    assert(sort(Array(3, 2)) === Array(2, 3))
  }

  test("例 351246") {
    assert(sort(Array(3, 5, 1, 2, 4, 6)) === Array(1, 2, 3, 4, 5, 6))
  }

  test("例 354162") {
    assert(sort(Array(3, 5, 4, 1, 6, 2)) === Array(1, 2, 3, 4, 5, 6))
  }

  test("例 654321") {
    assert(sort(Array(6, 5, 4, 3, 2, 1)) === Array(1, 2, 3, 4, 5, 6))
  }

  test("例 42856371") {
    assert(sort(Array(4, 2, 8, 5, 6, 3, 7, 1)) === Array(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("例 25302303") {
    assert(sort(Array(2, 5, 3, 0, 2, 3, 0, 3)) === Array(0, 0, 2, 2, 3, 3, 3, 5))
  }
}

class InsertionSortTest extends FunSuite with SortTest {
  val sort = Sort.insertionSort[Int] _
}

class SelectionSortTest extends FunSuite with SortTest {
  val sort = Sort.selectionSort _
}

class MergeSortTest extends FunSuite with SortTest {
  val sort = Sort.mergeSort _
}

class BubbleSortTest extends FunSuite with SortTest {
  val sort = Sort.bubbleSort _
}

class QuickSortTest extends FunSuite with SortTest {
  val sort = Sort.quickSort2 _
}

class CountingSortTest extends FunSuite with SortTest {
  val sort = Sort.countingSort _
}

class HeapSortTest extends FunSuite with SortTest {
  val sort = Sort.heapSort _
}

class BucketSortTest extends FunSuite with SortTest {

  import MyList._
  import Sort._

  // 最初に 0~1 にしますよ！！
  // 最大値は 10 の前提です！！
  val sort = (seq: Array[Int]) => {
    val results = Sort.bucketSort(seq.map(_ / 10.0))
    results.map(a => (a * 10).asInstanceOf[Int])
  }

  test("リストに追加") {
    val a, b = new Node(1)
    append(a, b)
    assert(a.next === b)
  }

  test("リストに追加 - 複数要素") {
    val a, b, c = new Node(1)
    append(a, b)
    append(a, c)
    assert(a.next.next === c)
  }

  test("リストのサイズ") {
    val a, b, c = new Node(1)
    assert(size(a) === 1)

    append(a, b)
    append(a, c)
    assert(size(a) === 3)
  }

  test("リストを配列に変換") {

    val a = new Node(1)
    val b = new Node(2)
    val c = new Node(3)
    assert(copyTo(a, new Array[Int](1)) === Array(1))

    append(a, b)
    append(a, c)
    assert(copyTo(a, new Array[Int](3)) === Array(1, 2, 3))
  }

  test("double") {
    assert(bucketSort(Array(0.78, 0.17, 0.39, 0.26, 0.72, 0.94, 0.21, 0.12, 0.23, 0.68)) ===
      Array(0.12, 0.17, 0.21, 0.23, 0.26, 0.39, 0.68, 0.72, 0.78, 0.94))
  }
}

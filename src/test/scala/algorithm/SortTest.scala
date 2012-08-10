package algorithm

import org.scalatest.FunSuite
import collection.mutable.ArrayBuffer

object Sort {
  def insertionSort(seq: Array[Int]): Array[Int] = {
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

  test("例 654321") {
    assert(sort(Array(6, 5, 4, 3, 2, 1)) === Array(1, 2, 3, 4, 5, 6))
  }

  test("例 42856371") {
    assert(sort(Array(4, 2, 8, 5, 6, 3, 7, 1)) === Array(1, 2, 3, 4, 5, 6, 7, 8))
  }
}

class InsertionSortTest extends FunSuite with SortTest {
  val sort = Sort.insertionSort _
}

class SelectionSortTest extends FunSuite with SortTest {
  val sort = Sort.selectionSort _
}

class MergeSortTest extends FunSuite with SortTest {
  val sort = Sort.mergeSort _
}

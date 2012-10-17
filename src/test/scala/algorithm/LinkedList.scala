package algorithm

import org.scalatest.FunSuite
import annotation.tailrec

/**
 * 番兵を使わない ver.
 * @tparam A
 */
case class LinkedList[A]() {

  class Node(_value: Option[A], var prev: Node = null, var next: Node = null) {
    def value = _value.get
  }

  /**
   * 先頭の要素
   */
  var head: Node = _

  def search(k: A): Node = {
    @tailrec
    def loop(x: Node): Node = {
      x match {
        case null => null
        case x if x.value == k => x
        case _ => loop(x.next)
      }
    }
    loop(head)
  }

  /**
   * 先頭に追加します
   */
  def insert(x: A) {
    val node = new Node(Some(x))

    node.next = head
    if (head != null) {
      head.prev = node
    }

    head = node
    node.prev = null
  }

  def delete(x: Node) {
    if (x.prev != null) {
      x.prev.next = x.next
    } else head = x.next

    if (x.next != null) {
      x.next.prev = x.prev
    }
  }
}


class LinkedListTest extends FunSuite {
  test("insert") {
    val l = LinkedList[Int]()
    l.insert(10)
    assert(l.head.value === 10)
    l.insert(20)
    assert(l.head.value === 20)
    assert(l.head.next.value === 10)
  }

  test("search") {
    val l = LinkedList[Int]()
    l.insert(10)
    l.insert(20)
    assert(l.search(10).value === 10)
    assert(l.search(20).value === 20)
  }

  test("search 見つからない場合") {
    val l = LinkedList[Int]()
    l.insert(10)
    assert(l.search(11) === null)
  }

  test("delete") {
    val l = LinkedList[Int]()
    (0 to 5).foreach(l.insert) // 5, 4, 3, 2, 1 なリスト
    l.delete(l.head.next) // 真ん中あたり削除
    assert(l.head.value === 5)
    assert(l.head.next.value === 3)

    l.delete(l.head.next) // 先頭削除

    // l.delete() // 末尾削除
  }

  test("delete - 値による削除") {
    val l = LinkedList[Int]()
    (0 to 5).foreach(l.insert) // 5, 4, 3, 2, 1 なリスト
    l.delete(l.head.next) // 真ん中あたり削除
    assert(l.head.value === 5)
    assert(l.head.next.value === 3)

    l.delete(l.head.next) // 先頭削除

    // l.delete() // 末尾削除
  }
}
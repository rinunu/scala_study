package algorithm

import org.scalatest.FunSuite
import annotation.tailrec

case class BinarySearchTree[A <% Ordered[A]]() {

  case class Node(key: A,
    var left: Node = null,
    var right: Node = null,
    var parent: Node = null)

  var root: Node = null

  def insert(key: A) {
    root match {
      case null => root = Node(key)
      case x => insert(x, key)
    }
  }

  def search(key: A): Option[Node] = search(root, key)

  def delete(z: Node) {
    //if (z.left == null) {
    //transplant(z, z.left)
    //}
  }

  /**
   * u をルートとする部分木の位置に v を移動する
   * u は v の子どもとなる
   */
  def transplant(u: Node, v: Node) {
    if (u.parent == null) {
      root = v
    } else if (u == u.parent.left) {
    } else {
      u.parent.right = v
    }
    if (v != null) {
      v.parent = v.parent
    }
  }

  override def toString(): String = {
    def f(n: Node, h: Int): String = {
      if (n != null) {
        val s = "-" * h + " " + n.key + "\n"
        s + f(n.left, h + 1) + f(n.right, h + 1)
      } else {
        ""
      }
    }

    f(root, 1)
  }

  @tailrec
  private def search(node: Node, key: A): Option[Node] = {
    if (node == null) {
      return None
    }

    key match {
      case _ if key == node.key => Some(node)
      case _ if key < node.key => search(node.left, key)
      case _ if key > node.key => search(node.right, key)
    }
  }

  @tailrec
  private def insert(node: Node, key: A) {
    if (key < node.key) {
      node.left match {
        case null => node.left = Node(key, parent = node)
        case y => insert(y, key)
      }
    } else {
      node.right match {
        case null => node.right = Node(key, parent = node)
        case y => insert(y, key)
      }
    }
  }
}

object BinarySearchTree {
  def apply[A <% Ordered[A]](values: A*): BinarySearchTree[A] = {
    val tree = BinarySearchTree[A]()
    values.foreach(tree.insert)
    tree
  }
}

class BinarySearchTreeTest extends FunSuite {
  test("insert") {
    val tree = BinarySearchTree[Int]()
    tree.insert(10)
    tree.insert(9) // left
    tree.insert(11) // right
    assert(tree.root.key === 10)
    assert(tree.root.left.key === 9)
    assert(tree.root.right.key === 11)
    assert(tree.root.left.parent === tree.root)
    assert(tree.root.right.parent === tree.root)
  }

  test("insert - 2階層以上") {
    val tree = BinarySearchTree[Int]()
    tree.insert(10)
    tree.insert(8) // left
    tree.insert(9) // right
    tree.insert(7) // right
    assert(tree.root.key === 10)
    assert(tree.root.left.key === 8)
    assert(tree.root.left.left.key === 7)
    assert(tree.root.left.right.key === 9)
  }

  test("search") {
    // insert は正しい前提
    val tree = BinarySearchTree(10, 8, 9, 7)

    assert(tree.search(7).get.key === 7)
    assert(tree.search(8).get.key === 8)
    assert(tree.search(9).get.key === 9)
    assert(tree.search(10).get.key === 10)
    assert(tree.search(11) === None)
  }

  test("toString - root") {
    // insert は正しい前提
    val tree = BinarySearchTree(5)
    assert(tree.toString ===
      """- 5
        | """.stripMargin)
  }

  test("toString - 1階層") {
    // insert は正しい前提
    val tree = BinarySearchTree(5, 2, 7)
    assert(tree.toString ===
      """- 5
        |-- 2
        |-- 7
      """.stripMargin)
  }

  test("toString - 2階層") {
    // insert は正しい前提
    val tree = BinarySearchTree(5, 2, 1, 3, 7, 6, 8)
    assert(tree.toString ===
      """- 1
        |- 5
        |-- 2
        |--- 1
        |--- 3
        |-- 7
        |--- 6
        |--- 8
      """.stripMargin)
  }

  /**
   * 単純な木を作る
   */
  def simpleTree = {
    // insert/search は正しい前提
    val tree = BinarySearchTree(5, 2, 1, 3, 7, 6, 8)
    // 想定通りの木になっている確認
    // - 5
    // -- 2
    // --- 1
    // --- 3
    // -- 7
    // --- 6
    // --- 8
    assert(tree.root.key === 5)
    assert(tree.root.left.key === 2)
    assert(tree.root.left.left.key === 1)
    assert(tree.root.left.right.key === 3)
    assert(tree.root.right.key === 7)
    assert(tree.root.right.left.key === 6)
    assert(tree.root.right.right.key === 8)
    tree
  }

  test("transplant - u がルート") {
    val tree = simpleTree
  }

  test("delete - 子を持たない場合") {
    val tree = simpleTree
    println(tree)
    tree.delete(tree.search(1).get)
    assert(tree.root.key === 5)
    assert(tree.root.left.key === 2)
    assert(tree.root.left.left === null) // 単純にコレが消える
    assert(tree.root.left.right.key === 3)
    assert(tree.root.right.key === 7)
    assert(tree.root.right.left.key === 6)
    assert(tree.root.right.right.key === 8)
  }
}



package algorithm

import org.scalatest.FunSuite

trait TreeBase[KEY] {
  def root: TreeBase.Node[KEY]
}

object TreeBase {

  trait Node[KEY] {
    def left: Node[KEY]

    def right: Node[KEY]

    def key: KEY
  }

}

case class TestTree[KEY]() extends TreeBase[KEY] {

  import TestTree._

  var root: Node[KEY] = null
}

object TestTree {

  case class Node[KEY](
    key: KEY,
    var left: TreeBase.Node[KEY] = null,
    var right: TreeBase.Node[KEY] = null)
    extends TreeBase.Node[KEY] {
  }

}

object TreeUtils {
  def show(node: TreeBase.Node[_]): String = {
    def show(node: TreeBase.Node[_], h: Int): Option[String] = {
      if (node == null) return None

      val s = Some(" " * (h * 2) + "- " + node.key.toString)
      val left = show(node.left, h + 1)
      val right = show(node.right, h + 1)
      Some(Seq(s, left, right).flatten.mkString("\n"))
    }

    show(node, 0).getOrElse("")
  }
}

class TreeUtilsTest extends FunSuite {
  test("show - root") {
    val t = TestTree[Int]()
    t.root = TestTree.Node(1)

    assert(TreeUtils.show(t.root) ===
      """- 1""".stripMargin)
  }

  test("show - h=2") {
    val t = TestTree[Int]()
    t.root = TestTree.Node(1)
    t.root.left = TestTree.Node(2)
    t.root.right = TestTree.Node(3)

    assert(TreeUtils.show(t.root) ===
      """- 1
        |  - 2
        |  - c""".stripMargin)
  }

  test("build") {
  }
}

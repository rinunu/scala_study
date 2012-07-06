package maza

import annotation.tailrec
import collection.mutable.ListBuffer
import collection.mutable

case class Solver() {
  type Pos = Maze.Pos
  type Path = Seq[Pos]

  // 同じ道は除く
  private def filter(hash: mutable.Set[Pos], aisles: Set[Pos]): Set[Pos] = {
    for (a <- aisles if !hash.contains(a)) yield a
  }

  def solve(maze: Maze): Path = {
    val nexts = ListBuffer[List[Pos]]()

    // すでに使用した道
    val hash = mutable.HashSet.empty[Pos]

    @tailrec
    def impl(): List[Pos] = {
      // Thread.sleep(1) // 重いの。。
      val path = nexts.head
      nexts.remove(0)
      println("cur", path.size)

      val head = path.head
      if (maze(head) == Maze.Goal) {
        println("goal!")
        path.reverse
      } else {
        hash += head
        val aisles = filter(hash, maze.aisles(head))
        // println("aisles", aisles)
        nexts ++= aisles.toSeq.map(_ :: path)
        impl()
      }
    }

    val s = maze.start
    nexts += List(s)
    impl()
  }

}

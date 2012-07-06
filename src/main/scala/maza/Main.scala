package maza

import io.Source

object Main extends App {
  val source = Source.fromFile("quiz.txt")
  val s = source.getLines().mkString("\n")
  val maze = Maze(s)
  println(maze.width, maze.height)
  val solver = Solver()
  val path = solver.solve(maze)

  val res = path.foldLeft(maze) {
    (maze, p) =>
      if (maze(p) == Maze.Space) {
        maze(p) = '$'
      } else {
        maze
      }
  }

  println(res)
}

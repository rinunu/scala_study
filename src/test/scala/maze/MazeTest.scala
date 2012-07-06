package maze

import org.scalatest.FunSuite
import maza.Maze

class MazeTest extends FunSuite {
  test("construct") {
    val maze = Maze( """|* S
                       |***""".stripMargin)

    assert(maze(0, 0) === '*')
    assert(maze(0, 1) === '*')
    assert(maze(1, 0) === ' ')
    assert(maze.width === 3)
    assert(maze.height === 2)
  }

  test("start") {
    val maze = Maze( """|***
                       |* *
                       |*S*""".stripMargin)
    assert(maze.start ===(1, 2))
  }

  test("goal") {
    val maze = Maze( """|***
                       |*G*
                       |*S*""".stripMargin)
    assert(maze.goal ===(1, 1))
  }

  test("aisles いける場所 - 下") {
    val maze = Maze( """|***
                       |*S*
                       |* *""".stripMargin)
    assert(maze.aisles((1, 1)) === Set((1, 2)))
  }

  test("aisles いける場所 - 右") {
    val maze = Maze( """|****
                       |*S *
                       |***""".stripMargin)
    assert(maze.aisles((1, 1)) === Set((2, 1)))
  }

  test("aisles いける場所 - 左") {
    val maze = Maze( """|****
                       |* S*
                       |****""".stripMargin)
    assert(maze.aisles((2, 1)) === Set((1, 1)))
  }

  test("aisles いける場所 - 上") {
    val maze = Maze( """|* *
                       |*S*
                       |***""".stripMargin)
    assert(maze.aisles((1, 1)) === Set((1, 0)))
  }

  test("aisles 複数") {
    val maze = Maze( """|* *
                       |*S*
                       |* *""".stripMargin)
    assert(maze.aisles((1, 1)) === Set((1, 0), (1, 2)))
  }

  test("update") {
    val maze = Maze( """|* *
                       |*S*
                       |* *""".stripMargin)

    val maze2 = maze((1, 2)) = '$'
    assert(maze2.toString ===
      """|* *
        |*S*
        |*$*""".stripMargin)
  }

}

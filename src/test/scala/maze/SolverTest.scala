package maze

import org.scalatest.FunSuite
import maza.{Solver, Maze}

class SolverTest extends FunSuite {
  test("簡単") {
    val maze = Maze(
      """|****
        |*S G*
        |****"""
        .stripMargin)

    val solver = Solver()
    val path = solver.solve(maze)
    assert(path === Seq((1, 1), (2, 1), (3, 1)))
  }

  test("簡単 2") {
    val maze = Maze(
      """|*****
        |*S*G*
        |*   *
        |*****"""
        .stripMargin)

    val solver = Solver()
    val path = solver.solve(maze)
    assert(path === Seq((1, 1), (1, 2), (2, 2), (3, 2), (3, 1)))
  }

  test("簡単 - 道が別れる") {
    val maze = Maze(
      """|*******
        |** ****
        |* S*G *
        |*     *
        |*******"""
        .stripMargin)

    val solver = Solver()
    val path = solver.solve(maze)
    assert(path === Seq((2, 2), (2, 3), (3, 3), (4, 3), (4, 2)))
  }

}

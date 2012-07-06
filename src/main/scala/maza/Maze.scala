package maza

object Maze {
  type Pos = (Int, Int)
  type Block = Char

  val Start = 'S'
  val Goal = 'G'
  val Wall = '*'
  val Space = ' '
}

case class Maze(s: String) {

  import Maze._

  private val lines = s.split("\n").toIndexedSeq

  override def toString: String = lines.mkString("\n")

  def apply(p: Pos): Block = {
    val (x, y) = p
    lines(y)(x)
  }

  def update(p: Pos, b: Block): Maze = {
    val (x, y) = p
    // 輪郭は update されない前提
    val res = for ((line, y2) <- lines.zipWithIndex) yield {
      if (y == y2) {
        line.substring(0, x) + b + line.substring(x + 1)
      } else {
        line
      }
    }
    Maze(res.mkString("\n"))
  }

  val width: Int = lines(0).size

  val height: Int = lines.size

  def start: Pos = find(Start)

  def goal: Pos = find(Goal)

  def find(s: Block): Pos = {
    val y = lines.indexWhere(_.contains(s))
    val x = lines(y).indexOf(s)
    (x, y)
  }

  /**
   * すすめる座標を返します
   */
  def aisles(current: Pos): Set[Pos] = {
    // 全面壁に囲まれている前提のアルゴリズムです
    def check(p: Pos): Option[Pos] = {
      val (x, y) = p
      this(p) match {
        case Start => Some(p)
        case Goal => Some(p)
        case Space => Some(p)
        case Wall => None
      }
    }

    val (x, y) = current
    val res = Set(
      check(x, y + 1),
      check(x, y - 1),
      check(x + 1, y),
      check(x - 1, y)
    )

    res.flatten
  }
}

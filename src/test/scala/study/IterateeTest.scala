/**
 * Iteratee
 *
 * http://d.hatena.ne.jp/tanakh/20100824
 *
 * http://d.hatena.ne.jp/mkotha/20111106/1320584724
 *
 * I/O 用の pattern.
 *
 * TODO 特徴
 *
 *
 * 抽象化
 * iteratee: アキュームレータ + 合成関数です
 * enumerator: 入力を生成し、 iteratee に与えるものです
 *
 * 計算の合成
 * - iteratee の合成
 * - (enumerator の合成もできますが、 IO が必要なので、ここでは割愛します。)
 *
 *
 * TODO scalaz にあるっぽい? IterV とか
 */

package study

import org.scalatest.FunSuite

/**
 * Stream を構成する1つの要素(stream 全体ではなく)
 */
sealed trait StreamG[+E]

case object Empty extends StreamG[Nothing]

final case class El[E](el: E) extends StreamG[E]

case object EOF extends StreamG[Nothing]

/**
 * iteratee
 *
 * stream のすべてを消費しなくても、計算を終了できます
 *
 * @tparam E 要素
 * @tparam A 計算結果
 */
sealed trait IterV[E, A] {

  /**
   * IterV はモナド
   */
  def flatMap[A1](f: A => IterV[E, A1]): IterV[E, A1]
}


final case class Done[E, A](x: A, str: StreamG[E]) extends IterV[E, A] {
  def flatMap[A1](f: A => IterV[E, A1]): IterV[E, A1] = {
    // 自身の処理がすでに Done なので、
    f(x) match {
      // 次の IterV が終わっているなら、それを返し
      case Done(x1, _) => Done(x1, str)
      // 次の IterV が継続なら、実行する
      case Cont(k) => k(str)
    }
  }
}

/**
 * 計算途中
 *
 * k は次の要素を計算するのかな?
 */
final case class Cont[E, A](k: StreamG[E] => IterV[E, A]) extends IterV[E, A] {
  def flatMap[A1](f: A => IterV[E, A1]): IterV[E, A1] = {
    // 自身を実行後に、 f を実行する IterV を返します
    Cont(str => k(str).flatMap(f))
  }
}

class IterateeTest extends FunSuite {

  /**
   * enumerator の例
   *
   * E の要素を生成し、 A を結果とする IterV に与えます。
   * 生成完了時は、その時点の IterV を返します。
   */
  def enum[E, A](iter: IterV[E, A], e: List[E]): IterV[E, A] = (iter, e) match {
    case (iter, Nil) => iter
    case (iter@(Done(_, _)), _) => iter
    case (Cont(k), x :: xs) =>
      enum(k(El(x)), xs)
  }

  /**
   * 処理を完結させ、計算結果を取得します
   */
  def run[E, A](iter: IterV[E, A]): A = iter match {
    case Done(x, _) => x
    // 処理中なら EOF を送って終了します
    case Cont(k) => k(EOF) match {
      case Done(x, _) => x
      case _ => sys.error("終わらないよ。。")
    }
  }


  // サンプルの iteratee 幾つか
  // 説明はテスト参照です
  def head[E]: IterV[E, Option[E]] = {
    def step(e: StreamG[E]): IterV[E, Option[E]] = e match {
      case El(e) => Done(Some(e), Empty)
      case Empty => Cont(step)
      case EOF => Done(None, EOF)
    }
    Cont(step)
  }

  def peek[E]: IterV[E, Option[E]] = {
    def step(e: StreamG[E]): IterV[E, Option[E]] = e match {
      case c@El(e) => Done(Some(e), c)
      case Empty => Cont(step)
      case EOF => Done(None, EOF)
    }
    Cont(step)
  }

  def drop[E](n: Int): IterV[E, Unit] = n match {
    case 0 => Done((), Empty)
    case n => {
      def step(e: StreamG[E]): IterV[E, Unit] = e match {
        case El(e) => drop(n - 1)
        case Empty => Cont(step)
        case EOF => Done((), EOF)
      }
      Cont(step)
    }
  }

  def length[E]: IterV[E, Int] = {
    def step(acc: Int)(e: StreamG[E]): IterV[E, Int] = e match {
      case El(e) => Cont(step(acc + 1))
      case Empty => Cont(step(acc))
      case EOF => Done(acc, EOF)
    }
    Cont(step(0))
  }

  test("先頭要素を消費して、先頭要素を返す iteratee") {
    val it = enum(head[Int], List(0, 1, 2))
    assert(it === Done(Some(0), Empty))
  }

  test("先頭要素を消費せず、先頭要素を返す iteratee") {
    val it = enum(peek[Int], List(0, 1, 2))
    assert(it === Done(Some(0), El(0)))
  }

  test("要素を n 個消費する iteratee") {
    val it = enum(drop[Int](2), List(0, 1, 2))
    // 消費していることの確認はまだできないの。。
    assert(it === Done((), Empty))
  }

  test("長さを返す iteratee") {
    val it = enum(drop[Int](2), List(0, 1, 2))
    assert(it === Done((), Empty))
  }

  /*
   * iteratee を合成して、別の iteratee を作れます。
   */

  test("iteratee の合成 1文字捨てて、次の文字を返す iteratee") {
    def drop1keep1[E]: IterV[E, Option[E]] = {
      drop(1).flatMap(a => head)
    }
    val iter = enum(drop1keep1[Int], List(0, 1, 2))
    assert(run(iter) === Some(1))
  }

}

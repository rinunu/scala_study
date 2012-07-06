import com.sun.xml.internal.bind.v2.model.core.MaybeElement
import org.scalatest.FunSuite


trait Maybe[A] {
  // 文脈を保たないといけない
  def flatMap[B](f: A => Maybe[B]): Maybe[B]

  def map[B](f: A => B): Maybe[B]
}

case class Nothing[A]() extends Maybe[A] {
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = Nothing()

  def map[B](f: A => B): Maybe[B] = Nothing()
}

case class Just[A](x: A) extends Maybe[A] {
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = f(x)

  def map[B](f: A => B): Maybe[B] = Just(f(x))
}


class MonadTest extends FunSuite {
  test("maybe") {
    // モナド ma を a => mb に食わせたい

    assert(Just(3).flatMap(x => Just(x + 1)) === Just(4))

    assert(Just("smile").flatMap(x => Just(x + " :)")) === Just("smile :)"))

    assert(Nothing[String]().flatMap(x => Just(x + " :)")) === Nothing())

    assert(Just(3).flatMap(a => Nothing[Int]()) === Nothing())
  }

  test("鳥さんの例") {
    type Birds = Int
    type Pole = (Birds, Birds)

    def landLeft(n: Birds, pole: Pole): Maybe[Pole] = {
      val (left, right) = pole
      if (((left + n) - right).abs < 4) Just(left + n, right)
      else Nothing()
    }

    def landRight(n: Birds, pole: Pole): Maybe[Pole] = {
      val (left, right) = pole
      if ((left - (right + n)).abs < 4) Just(left, right + n)
      else Nothing()
    }

    // こんな風に使えるよ
    assert(landLeft(2, (0, 0)) === Just(2, 0))
    assert(landRight(1, (1, 2)) === Just(1, 3))
    assert(landRight(-1, (1, 2)) === Just(1, 1))

    // 失敗すると Nothing() が返るよ
    assert(landLeft(10, (0, 3)) === Nothing())

    // flatMap を使えば、モナドへ、「通常の値をとってモナドを返す関数」を適用できるよ
    assert(landRight(1, (0, 0)).flatMap(landLeft(2, _)) === Just(2, 1))
    assert(Nothing[Pole]().flatMap(landLeft(2, _)) === Nothing())

    // モナドと flatMap を使えば、途中で失敗するケースも扱える
    assert(Just(0, 0).flatMap(landLeft(1, _)).flatMap(landRight(4, _)).flatMap(landLeft(-1, _)).flatMap(landRight(-2, _)) ===
      Nothing())
  }

  test("for 記法") {
    // flatMap が入れ子になってる
    assert(
      Just(3).flatMap(x =>
        Just("!").flatMap(y =>
          Just(x + y))) === Just("3!"))

    // for 内包表記を使って書けるよ
    val res = for {
      x <- Just(3)
      y <- Just("!")
    } yield Just(x + y)

    assert(res === Just("3!"))
  }
}

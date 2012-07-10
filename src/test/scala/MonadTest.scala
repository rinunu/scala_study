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
    // モナド値を連鎖させることができる
    // 上の flatMap().flatMap との違いは?
    assert(
      Just(3).flatMap(x =>
        Just("!").flatMap(y =>
          Just(x + y))) === Just("3!"))

    // for 内包表記を使って書けるよ
    val res = for {
      x <- Just(3)
      y <- Just("!")
    } yield x + y

    assert(res === Just("3!"))
  }

  test("パターンマッチ") {
    // Just だと filter が必要といわれるので、 Some で。

    val res = for {
      (x :: xs) <- Some(1 :: 2 :: Nil)
    } yield x
    assert(res === Some(1))

    // Haskell の場合は fail が使われるけど、 Scala は TODO
    // None になったけど、なんで?
    val res2 = for {
      (x :: xs) <- Some(List.empty)
    } yield {
      println(x, xs)
      x
    }
    assert(res2 === Nil)
  }

  test("モナドの満たすべき規則") {
    // 前提
    //
    // List もモナド
    //
    // 何かするモナド関数 f, g があります
    val f = (x: Int) => List(x, -x) // 適当に
    val g = (x: Int) => List(x * 3, x * 2) // 適当に
    // 何らかの値
    val x = 1
    // そのモナド
    val m = List(x)


    // ここから規則

    // モナドは、値からモナド値を生成するコンストラクタと flatMap を持っており、
    // 以下の 3つの式を満たします。

    assert(constructList(x).flatMap(f) === f(x))
    // 意味: コンストラクタは x を再現できる最小限のモナドを作るものです。
    // 最低限のモナド値に f を適用することと、 x そのものに f を適用した結果は == でなければいけないのです
    //
    // 左恒等性というそうです

    assert(m.flatMap(constructList) === m)
    // 意味:  コンストラクタは、値をその値を再現できる最小限のモナド値を作るものです。
    // flatMap にてモナド値からこの値を取り出し、再度コンストラクタで最小限のモナド値を作るので、
    // 結果はもとのものと同じになってほしいのです。
    //
    // 右恒等性というそうです

    // 左恒等性と右恒等性を満たすことで、「コンストラクタが値から最小限のモナド値を作る」といえるようになる
    // ってことですか？

    (m.flatMap(f).flatMap(g) === m.flatMap(f(_).flatMap(g)))
    // 意味: 入れ子にしてるとか、してないとかは関係なく、同じ意味なんだよ〜
    // ってことですか？
    //
    // 結合法則っていうそうです
  }

  test("モナドの満たすべき規則 - 別の説明") {
    // 前提
    //
    // List もモナドです。
    //
    // モナド関数 f, g を関数合成(f してから g する関数を作る)する <=< な演算子があるとします。
    //
    // 以下のモナド関数があるとします
    val f = (x: Int) => List(x, -x)
    val g = (x: Int) => List(x * 3, x * 2)
    val h = f <=< g

    // このとき、規則は以下のように説明できるそうです
    // みためちょっとわかりやすいかもー？

    // 左恒等性
    // (f <=< returnList) === f
    assert((f <=< constructList)(3) === f(3))

    // 右恒等性
    // (returnList <=< f) === f
    assert((constructList <=< f)(3) === f(3))

    // 結合法則
    // (f <=< (g <=< h)) === ((f <=< g) <=< h)
    assert((f <=< (g <=< h))(3) === ((f <=< g) <=< h)(3))
  }

  // ここから先、説明で使う部品です

  /**
   * リストコンストラクタ
   */
  val constructList = (a: Int) => List(a)

  class RichListMonadFunc(f: Int => List[Int]) {
    /**
     * List モナド関数を合成
     */
    def <=<(g: Int => List[Int]): Int => List[Int] =
      x => g(x).flatMap(f)
  }

  implicit def wrap(f: Int => List[Int]): RichListMonadFunc =
    new RichListMonadFunc(f)
}

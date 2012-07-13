import org.scalatest.FunSuite

/**
 * http://togetter.com/li/17462
 *
 */
class TypeConstructorTest extends FunSuite {

  /**
   * A という type constructor を受け取る
   *
   * A には例えば Seq などを渡せる
   * Seq[Int] はだめ。 Seq[Int] は type であり、 type constructor ではないので。
   */
  class Foo[A[B], B] {
    def a: A[B] = sys.error("")

    def b: B = sys.error("")
  }

  test("") {
    val a = new Foo[Seq, Int]()
    val b = a.a
  }
}

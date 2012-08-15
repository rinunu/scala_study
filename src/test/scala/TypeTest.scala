import org.scalatest.FunSuite

/**
 * type についていろいろお試し
 */

class TypeTest extends FunSuite {

  class Foo[A, B] {
    def a: A = sys.error("")
    def b: B = sys.error("")
  }

  case class Bar[A, B, C[A, B]](foo: C[A, B]) {
  }

  test("") {
    val a = new Bar(new Foo[Int, String]())
  }


}

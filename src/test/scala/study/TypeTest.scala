package study

import org.scalatest.FunSuite

class TypeTest extends FunSuite {

  class Foo[+A]() {

    /**
     * これだと convariant type A が contravariant position で使われているよ
     * というエラーになる
     * {{{
     * def set(x: A) {
     * }
     * }}}
     *
     * なぜなら、こんなことをすると破綻するため
     * {{{
     * val a = Foo[Int]
     * val b: Foo[Any] = a
     * val b.set(String)
     * }}}
     */
  }

  test("variance") {

  }
}

package study

import org.scalatest.FunSuite
import util.parsing.combinator._

class ParserCombinatorTest extends FunSuite {

  /**
   * expr ::= term {"+" term | "-" term}.
   * term ::= factor {"*" factor | factor}.
   * factor ::= floatingPointNumber | "(" expr ")".
   */
  class Arith extends JavaTokenParsers {
    def expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)

    def term: Parser[Any] = factor ~ rep("*" ~ factor | "/" ~ factor)

    def factor: Parser[Any] = floatingPointNumber | "(" ~ expr ~ ")"
  }

  test("a") {
    val a = new Arith
    println(a.parseAll(a.expr, "2 * (3 + 7)"))
  }

}

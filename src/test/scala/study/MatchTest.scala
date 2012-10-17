package study

import org.scalatest.FunSuite
import util.matching.Regex

object EMail {
  /**
   * user, domain に分解します(extractor)
   */
  def unapply(s: String): Option[(String, String)] = {
    val parts = s split "@"
    // func((a, b)) => func(a, b) と省略できます
    if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
}

class MatchTest extends FunSuite {
  test("リテラルとのパターンマッチ") {
    1 match {
      case 1 => assert(true)
      case 2 => fail()
    }
  }

  test("パターンマッチ") {
    // アドレスが test.com, test2.com であり、正しいフォーマットのアドレスの時は true
    def isValidAddress(addr: String): Boolean = {
      addr match {
        case EMail(_, "test.com") => true
        case EMail(_, "test2.com") => true
        case _ => false // その他
      }
    }

    assert(isValidAddress("foo@test.com"))
    assert(isValidAddress("foo@test2.com"))
    assert(!isValidAddress("foo@test3.com"))
    assert(!isValidAddress("footest2.com"))
  }

  test("パターンマッチと分解") {
    // アドレスが test.com にマッチするときに、そのユーザ部分を返します
    // Option は Some(値を持つ場合) または None(null みたいなもの) のどちらかである値を表します
    def getValidUser(addr: String): Option[String] = {
      addr match {
        case EMail(user, "test.com") => Some(user)
        case _ => None
      }
    }

    assert(getValidUser("foo@test.com") === Some("foo"))
    assert(getValidUser("foo@test3.com") === None)
  }

  // ここからは応用

  test("case class は自動的に extractor を定義します") {
    // 実際には extractor ではないらしい? case class のほうが最適化されやすいそうな(コップ本)
    // でも、ここでは extractor であるとして記述します。
    case class User(firstName: String, lastName: String, age: Int)

    val users = Array(
      User("taro", "yamada", 18),
      User("jiro", "yamada", 17),
      User("ichiro", "tanaka", 18),
      User("goro", "yamada", 18))

    // 18歳の yamada さんだけ抜き出します

    val yamada17List = users.filter(_ match {
      case User(_, "yamada", 18) => true
      case _ => false
    })

    assert(yamada17List === Array(users(0), users(3)))
  }

  test("List 用の extractor も標準ライブラリに入ってます") {
    val list = 1 :: 2 :: 3 :: Nil

    // 先頭と残りの要素に分解します
    list match {
      // :: という名前の case class が定義されています
      case ::(head, tail) =>
        assert(head === 1)
        assert(tail === List(2, 3))
      case _ =>
        fail()
    }

    // パターン部分はメソッド呼び出しと同じように、中置演算子っぽく書くこともできます
    // :: の場合は、こう書くのが普通です。
    list match {
      // :: という名前の case class が定義されています
      case head :: tail =>
      case _ =>
    }
  }

  test("パターンがオブジェクトの場合") {
    // パターンには unapply を持ったオブジェクトを使える。 つまりこういうのもできる
    class Foo {
      def unapply(s: String): Option[String] = {
        if (s == "ok") Some("OK") else None
      }
    }

    val Ok = new Foo

    "ok" match {
      case Ok(_) =>
      case _ => fail()
    }

    // このパターンが便利なのは Regex
    val EMailRe = new Regex("([a-z]+)@([a-z.]+)")
    "test@test.com" match {
      case EMailRe(user, domain) =>
        assert(user === "test")
        assert(domain === "test.com")
      case _ => fail()
    }
  }

  //
  // 終端識別子 ( [パターンリスト [,]] ) ケースクラスまたはケースシングルトンマッチ
  // 終端識別子 ( [パターンリスト ,] _ * )

  // コレ以外に unapplySeq もありますが、省略します。
}

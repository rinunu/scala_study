/**
 * http://d.hatena.ne.jp/seratch2/20111126/1322309305
 */

import org.scalatest.FunSuite
import util.control.Exception._

class ExceptionTest extends FunSuite {

  case class Exception0() extends RuntimeException

  case class Exception1() extends RuntimeException

  test("Catch を明示的に生成") {
    // Catch は以下のロジックを保持します
    // - catch した時にどうするか
    // - finally 時にどうするか

    val cat = new Catch({
      case e: Exception0 => "catch"
    }) andFinally {
    }

    // catch を使用してロジックを実行します
    val a = cat {
      "a"
    }
    assert(a === "a")

    val b = cat {
      throw Exception0()
    }
    assert(b === "catch")

    intercept[Exception1] {
      cat {
        throw Exception1()
      }
    }
  }

  test("allCatch: すべて catch する Catch を作ります") {
    // キャッチした際のロジックは withApply で指定します
    val cat = allCatch.withApply {
      e => "catch"
    }

    assert(cat("a") === "a")

    assert(cat(throw Exception0()) === "catch")

    // InterruptedException 等は除外されます
    intercept[InterruptedException] {
      cat {
        throw new InterruptedException()
      }
    }
  }

  test("catching: 指定した例外をキャッチする Catch を生成します") {
    // allCatch と同様かと思われます。
    val cat = catching(classOf[Throwable]).withApply {
      e => "catch"
    }

    assert(cat("a") === "a")
  }

  test("option に変換します") {
    val cat = catching(classOf[Throwable])

    // 例外が発生しない場合
    val a = cat.opt {
      "a"
    }
    assert(a === Some("a"))

    // 想定していた例外が発生した場合
    val b = cat.opt {
      throw Exception0()
    }
    assert(b === None)

    // 想定していない例外が発生した場合
    intercept[Exception1] {
      cat.opt {
        throw Exception1()
      }
    }
  }

  test("either に変換します") {
    // 基本的に option と同じかと思われます
    val cat = new Catch({
      case e: Exception0 => "catch"
    })

    // 例外が発生しない場合
    val a = cat.either {
      "a"
    }
    assert(a === Right("a"))

    val b = cat.either {
      throw Exception0()
    }
    assert(b === Left(Exception0()))
  }

  test("andFinally: ") {
    var executedFinally = false
    val cat = catching(classOf[Exception0]) andFinally {
      executedFinally = true
    }

    intercept[Exception1] {
      val a = cat {
        throw Exception1()
      }
    }
    assert(executedFinally)
  }

  test("ignoring: 指定した例外を無視します") {
    ignoring(classOf[Exception0]).either {
      throw Exception0()
    }

    // 何も指定しないと何も無視しません
    intercept[Exception0] {
      ignoring().either {
        throw Exception0()
      }
    }
  }
}

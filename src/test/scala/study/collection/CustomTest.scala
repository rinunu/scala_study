package study.collection

import org.scalatest.FunSuite
import scala.collection.{mutable, TraversableLike}


class CustomTest extends FunSuite {

  /**
   * ~Like が各種メソッドの実装を行なっています。
   */
  class MyList[Elem] extends TraversableLike[Elem, MyList[Elem]] {

    def newBuilder: mutable.Builder[Elem, MyList[Elem]] = {
      null
    }


    def foreach[U](f: Elem => U) {

    }

    def seq: TraversableOnce[Elem] = null
  }

  test("") {
    val list = new MyList[String]
  }

}

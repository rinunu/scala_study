import org.scalatest.FunSuite


class StateTest extends FunSuite {
  // スタックの例

  type Stack = List[Int]


  test("純粋な普通の関数を使う場合") {
    def pop(stack: Stack): (Int, Stack) = stack match {
      case x :: xs => (x, xs)
    }

    def push(a: Int, xs: Stack): (Unit, Stack) =
      ((), a :: xs)

    def stackManip(stack: Stack): (Int, Stack) = {
      // こんな風に状態を変更していくのはめんどくさい
      val ((), newStack1) = push(3, stack)
      val (a, newStack2) = pop(newStack1)
      pop(newStack2)
    }

    assert(stackManip(List(5, 8, 2, 1)) ===
      (5, List(8, 2, 1)))
  }

  // 状態付き計算
  // S => (A, S)
  //
  // 上記の push 以外は状態付き計算、 push は a を部分適用すれば状態付き計算

  // State モナド
  case class State[S, A](runState: S => (A, S)) {
    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      val h = runState
      State(s => {
        val (a, newState) = h(s) // this のモナド値から結果を取り出し、
        val State(g) = f(a) // 渡された関数を適用し、
        g(newState) // 新しいモナド値を作成する
      })
    }

    // TODO init がない代わりに、 map があるの?
    def map[B](f: A => B): State[S, B] =
      flatMap(a => State(f(a)))
  }

  object State {
    def apply[S, A](x: A): State[S, A] = State(s => (x, s))
  }

  test("State モナドを使う場合") {
    def pop: State[Stack, Int] =
      State(a =>
        a match {
          case x :: xs => (x, xs)
        })

    //
    def push(a: Int): State[Stack, Unit] =
      State(xs => ((), a :: xs))

    // flatMap ver.
    def stackManip: State[Stack, Int] = {
      push(3).flatMap { a =>
        pop.flatMap { b =>
          pop.map { c => c}
        }
      }
    }

    // for ver.
    def stackManip2: State[Stack, Int] = for {
      _ <- push(3)
      _ <- pop
      a <- pop
    } yield a

    assert(stackManip.runState(List(5, 8, 2, 1)) ===
      (5, List(8, 2, 1)))

    assert(stackManip2.runState(List(5, 8, 2, 1)) ===
      (5, List(8, 2, 1)))

    // ちょっと複雑な例
    // if するなら withFilter も必要。 とはいえ、 else かけないと思うのです。。
    // なので、直感的にかけま。。せんでした。
    def stackStuff: State[Stack, Unit] = for {
      _ <- pop.flatMap { a =>
        if (a == 5) push(5)
        else for {
          _ <- push(3)
          _ <- push(8)
        } yield ()
      }
    } yield ()

    assert(stackStuff.runState(List(9, 0, 2, 1, 0)) ===
      ((), List(8, 3, 0, 2, 1, 0)))


  }

}

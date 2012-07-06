package study.actor

object ActorMain {

  def main(args: Array[String]) {
    for (i <- 0 to 20) {
      val a = new MyActor("a" + i)
      a.start
      a ! "test"
    }
  }

}


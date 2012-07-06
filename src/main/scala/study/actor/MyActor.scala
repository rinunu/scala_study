package study.actor
import scala.actors.Actor

class MyActor(name: String) extends Actor {
  override def act {
    println("%s act(%s)".format(name, threadName))
    try {
      react {
        case s: String =>
          println("%s react(%s)".format(name, threadName))
          act
      }
    } finally {
      println("%s finally(%s)".format(name, threadName))
    }
  }

  def threadName = Thread.currentThread.getName
}

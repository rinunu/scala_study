import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql._
import extended.ExtendedProfile
import org.scalaquery.session.{Session, Database}

object Users extends Table[(Int, String, Option[String])]("users") {
  def id = column[Int]("id", O NotNull)

  def first = column[String]("first", O Default "NFN", O DBType "varchar(64)")

  def last = column[Option[String]]("last")

  def * = id ~ first ~ last
}

object Books extends Table[(Int, String)]("books") {
  def id = column[Int]("id", O.NotNull)

  def title = column[String]("title", O.NotNull)

  def * = id ~ title
}

object Tmp {
  val driver: ExtendedProfile = null

  import driver.Implicit._

  def foo() {

    val query = for (b <- Books) yield b.title ~ b.id

    println(query.selectStatement)

    val db = Database.forURL("jdbc:mysql://localhost/memory?user=root&password=xxx", driver = "org.scalaquery.ql.extended.MySQLDriver")
    val list = db.withSession {
      implicit session: Session =>
        query.to[Array]()
    }

    for (i <- list) {
      println(i)
    }
  }

  def main(args: Array[String]) {
    foo()
  }


}

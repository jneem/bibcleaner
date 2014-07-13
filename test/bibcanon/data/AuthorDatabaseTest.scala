package bibcanon.data

import bibcanon._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import bibtex.Name
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import scala.slick.driver.H2Driver
import scala.slick.jdbc.JdbcBackend.Database
import java.sql.DriverManager

@RunWith(classOf[JUnitRunner])
class AuthorDatabaseTest extends FlatSpec with Checkers {
  behavior of "AuthorDatabase"

  val DB_URL = "jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1"
  val DB_DRIVER ="org.h2.Driver"

  // I'm not sure why this is needed, but java doesn't find the driver otherwise...
  DriverManager.registerDriver(new org.h2.Driver)
  val connection = Database.forURL(DB_URL, DB_DRIVER)
  val db = new DataAccessLayer(H2Driver, connection)
  db.create()

  it should "allow insertion of authors" in {
    val myName = Name("Joe", "Neeman")
    val me = Person(myName)
    db.add(me)

    val results = db.findAll(me) map ((x: IdPerson) => x.name)
    assert(results === List(myName))
  }
}


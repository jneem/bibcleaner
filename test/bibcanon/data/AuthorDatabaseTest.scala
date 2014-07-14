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

  def withCleanDatabase(block: => Unit) = {
    db.create()
    block
    db.drop()
  }

  def checkNames(query: Name, expected: Set[Name]) = {
    val results = db.findAll(Person(query)) map (_.name)
    assert(results.toSet === expected)
  }

  it should "allow insertion of authors" in {
    withCleanDatabase {
      val myName = Name("Joe", "Neeman")
      db.add(Person(myName))

      checkNames(myName, Set(myName))
    }
  }

  it should "return only canonical authors" in {
    withCleanDatabase {
      val myName = Name("Joe", "Neeman")
      val myShortName = Name("J", "Neeman")
      val myTypoName = Name("J", "Meeman")
      val me = db.add(Person(myName))
      val meShort = db.add(Person(myShortName))
      val meTypo = db.add(Person(myTypoName))

      checkNames(myName, Set(myName, myShortName))
      checkNames(myTypoName, Set(myTypoName))

      db.setCanonical(meTypo, me)
      db.setCanonical(meShort, me)

      checkNames(myName, Set(myName))
      checkNames(myTypoName, Set(myName))
    }
  }
}


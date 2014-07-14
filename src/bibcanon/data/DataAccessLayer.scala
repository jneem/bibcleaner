package bibcanon.data

import concurrent._
import bibcanon._
import ExecutionContext.Implicits.global
import collection.mutable.{ HashMap => MutHashMap, Set => MutSet, MultiMap }
import java.text.Normalizer
import bibtex.BibtexEntry
import bibtex.Name
import scala.slick.driver.JdbcProfile
import scala.slick.jdbc.JdbcBackend.Database

trait Profile {
  val profile: JdbcProfile
  val db: Database
}

class DataAccessLayer(override val profile: JdbcProfile, override val db: Database)
  extends AuthorDatabase with PublicationDatabase with Profile {

  import profile.simple._

  private lazy val ddls = authorTable.ddl ++ authorCanonTable.ddl

  def create(): Unit = {
    db withSession { implicit session =>
      ddls.create
    }
  }

  def drop(): Unit = {
    db withSession { implicit session =>
      ddls.drop
    }
  }
}

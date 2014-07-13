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

/**
 * Notes on duplicates authors:
 * - we allow the insertion of a new author even if the name given already
 *   exists (because names need not be unique).
 * - when inserting a new author with the same (or similar) name as an existing author,
 *   we flag the similarity as requiring manual resolution. If the manual resolution
 *   says that they are really the same person, we select one entry as the canonical
 *   one, and store that fact. For example, "Joe Neeman" could be the canonical entry
 *   and "J Neeman" could be the non-canonical one. By pointing "J Neeman" at
 *   "Joe Neeman," we ensure that future lookups to "J Neeman" return the canonical
 *   entry.
 * - the relationship between canonical and non-canonical entries can be many-to-one.
 * - if there are multiple canonical entries for a given name, we need to somehow
 *   add manual resolution to publications.
 */

trait AuthorDatabase extends Utils { this: Profile =>

  import profile.simple._

  class AuthorTable(tag: Tag) extends Table[(Int, String, String, String, String, String, String)](tag, "AUTHORS") {
    def id = column[Int]("AUTHOR_ID", O.PrimaryKey, O.AutoInc)
    def firstName = column[String]("NAME_FIRST")
    def vonName = column[String]("NAME_VON")
    def lastName = column[String]("NAME_LAST")
    def jrName = column[String]("NAME_JR")
    def normalizedLastName = column[String]("NORMALIZED_LAST")
    def arxivId = column[String]("ARXIV_ID")

    def * = (id, firstName, vonName, lastName, jrName, normalizedLastName, arxivId)
  }
  
  val authorTable = TableQuery[AuthorTable]

  class AuthorCanonicalizationTable(tag: Tag) extends Table[(Int, Int)](tag, "AUTHOR_CANON") {
    def canonicalId = column[Int]("CANONICAL_ID")
    def subordinateId = column[Int]("SUBORDINATE_ID")

    def canonicalKey = foreignKey("CANONICAL_FK", canonicalId, authorTable)(_.id)
    def subordinateKey = foreignKey("SUBORDINATE_FK", subordinateId, authorTable)(_.id)

    def * = (canonicalId, subordinateId)
  }

  val authorCanonTable = TableQuery[AuthorCanonicalizationTable]

  def authorQueryExactName(p: Person) = {
    for {
      a <- authorTable if (a.firstName === p.name.first
                           && a.vonName === p.name.von
                           && a.lastName === p.name.last
                           && a.jrName === p.name.jr)
    } yield a
  }

  def authorQueryLastName(p: Person) = {
    for {
      a <- authorTable if a.normalizedLastName === normalize(p.name.last)
    } yield a
  }

  def authorQueryLastNameCanonicalized(p: Person) = {
    // TODO: uniqueify the result.
    for {
      a <- authorQueryLastName(p)
      canon <- authorCanonTable if canon.subordinateId === a.id
      b <- authorTable if canon.canonicalId === b.id
    } yield b
  }

  // TODO: make private
  def makePerson(idVal: Int, nameVal: Name, arxiv: String): IdPerson = {
    new IdPerson {
      def id = idVal
      def name = nameVal
      def arXivId = if (arxiv == "") None else Some(arxiv)
    }
  }

  // Convenience form, for applying straight to the tuple out of the database.
  private def makePersonLong(id: Int,
                             first: String, von: String, last: String, jr: String,
                             norm: String,
                             arxiv: String): IdPerson = {
    makePerson(id, Name(first, von, last, jr), arxiv)
  }
  // TODO: make private
  val makePersonT = (makePersonLong _).tupled

  /**
   * Returns all of the authors that have the given last name.
   * TODO: make private
   */
  def getAuthors(s: String): List[IdPerson] = {
    val tuples = for {
      a <- authorTable if a.normalizedLastName === normalize(s)
    } yield a
    db.withSession { implicit session =>
      tuples.list() map makePersonT
    }
  }

  /**
   * Returns all of the authors that match the given name.
   *
   * They must match the last name exactly, and then match the
   * rest of the name according to the Name.matches function.
   * TODO: make private
   */
  def getAuthors(n: Name): List[IdPerson] =
    getAuthors(n.last) filter (_.name matches n)

  /**
   * Finds all matches for the given author.
   *
   * The matches are not in any particular order.
   */
  def findAll(p: Person): List[IdPerson] = {
    val all = getAuthors(p.name)
    p.arXivId match {
      case None => all
      case Some(aId) => all filter (_.arXivId.forall(_ == aId))
    }
  }

  /**
   * Adds an author to the database.
   *
   * The author will be assigned a new unique id.
   */
  def add(p: Person): Unit = {
    // TODO: check that the person doesn't already exist.
    val norm = normalize(p.name.last)

    val cols = authorTable map (c => (c.firstName, c.vonName, c.lastName, c.jrName,
                                      c.normalizedLastName, c.arxivId))

    db.withSession { implicit session =>
      cols += ((p.name.first, p.name.von, p.name.last, p.name.jr,
                norm, p.arXivId.getOrElse("")))
    }
  }
}

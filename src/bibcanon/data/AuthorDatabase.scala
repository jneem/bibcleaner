package bibcanon.data

import concurrent._
import bibcanon._
import ExecutionContext.Implicits.global
import org.slf4j.LoggerFactory
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

  private def logger = LoggerFactory.getLogger(this.getClass)

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

  private def authorQueryExactName(p: Person) = {
    for {
      a <- authorTable if (a.firstName === p.name.first
                           && a.vonName === p.name.von
                           && a.lastName === p.name.last
                           && a.jrName === p.name.jr)
    } yield a
  }

  private def authorQueryLastName(lastName: String) = {
    for {
      a <- authorTable if a.normalizedLastName === normalize(lastName)
    } yield a
  }

  /**
   * Return the concatenation of
   * - the canonicalized versions of the given authors, and
   * - those given authors that do not have a canonicalized version
   */
  private def canonicalize(authors: List[IdPerson]): List[IdPerson] = {
    def canonicalizeOne(id: Int) = {
      val canon = for {
        c <- authorCanonTable if c.subordinateId === id
        a <- authorTable if c.canonicalId === a.id
      } yield a

      val noCanon =  for {
        (a, c) <- (authorTable leftJoin authorCanonTable on (_.id === _.subordinateId))
                   if a.id === id && c.subordinateId.isNull
      } yield a

      canon ++ noCanon
    }

    val authorIds = authors map (_.id)
    val queries = authors map (_.id) map canonicalizeOne
    val query = queries reduce (_ ++ _)

    db withSession { implicit session =>
      query.list().distinct map makePersonT
    }
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
   * We filter out matches that were deemed to be versions of
   * some canonical entry.
   */
  def findAll(p: Person): List[IdPerson] = {
    val all = getAuthors(p.name.last) filter (_.name matches p.name)
    val canon = canonicalize(all)
    p.arXivId match {
      case None => canon
      case Some(aId) => canon filter (_.arXivId.forall(_ == aId))
    }
  }

  def setCanonical(subordinate: IdPerson, canonical: IdPerson) = {
    db withSession { implicit session =>
      // Check that `canonical` is not itself subordinate to someone, and that
      // `subordinate` is not itself the canonical version of someone.
      val canonicalQuery = for { c <- authorCanonTable if c.subordinateId === canonical.id } yield c
      val canonicals = canonicalQuery.list()
      val subordinateQuery = for { c <- authorCanonTable if c.canonicalId === subordinate.id } yield c
      val subordinates = subordinateQuery.list()

      if (canonicals.isEmpty && subordinates.isEmpty) {
        authorCanonTable += (canonical.id -> subordinate.id)
      } else if (!canonicals.isEmpty) {
        logger.warn(s"Tried to make person ${canonical.id} a canonical version of ${subordinate.id}, " +
                    s"but ${canonical.id} is already subordinate to ${canonicals}")
      } else {
        logger.warn(s"Tried to make person ${canonical.id} a canonical version of ${subordinate.id}, " +
                    s"but ${subordinate.id} is already a canonical version of ${subordinates}")
      }
    }
  }

  /**
   * Adds an author to the database.
   *
   * The author will be assigned a new unique id, and the author with her new
   * id is then returned.
   */
  def add(p: Person): IdPerson = {
    val norm = normalize(p.name.last)

    val cols = authorTable map (c => (c.firstName, c.vonName, c.lastName, c.jrName,
                                      c.normalizedLastName, c.arxivId))
    val idCol = authorTable map (_.id)

    val result = db.withSession { implicit session =>
      val id = (cols returning idCol) += ((p.name.first, p.name.von, p.name.last, p.name.jr,
                                           norm, p.arXivId.getOrElse("")))
      (for { a <- authorTable if a.id === id } yield a).list()
    }
    makePersonT(result.head)
  }
}

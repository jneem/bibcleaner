package bibcanon

import concurrent._
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

/**
 * A database is a collection of authors, publications, journals, etc. with the
 * appropriate links between them and various lookup tables.
 */
class PublicationDatabase(val profile: JdbcProfile, db: Database) {

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

  private def makePerson(idVal: Int, nameVal: Name, arxiv: String): IdPerson = {
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
  private val makePersonT = (makePersonLong _).tupled

  class PublicationTable(tag: Tag) extends Table[(Int, String, String, String, Int, Int)](tag, "PUBLICATIONS") {
    def id = column[Int]("PUB_ID", O.PrimaryKey, O.AutoInc)
    def clazz = column[String]("CLASS") // The type of publication (e.g. "article")
    def title = column[String]("TITLE")
    def normalizedTitle = column[String]("NORMALIZED_TITLE")
    def year = column[Int]("YEAR")
    def venue = column[Int]("VENUE") // FIXME: make this a foreign key

    def * = (id, clazz, title, normalizedTitle, year, venue)
  }
  
  val publicationTable = TableQuery[PublicationTable]
  
  private def makePublication(id: Int, clazz: String, title: String, norm: String, year: Int, venue: Int)(authors: Seq[IdPerson]) = {
    if (clazz == "article") {
      Article(id, title, authors, Some(year), None, None)
    } else if (clazz == "book") {
      Book(id, title, authors, Some(year))
    } else throw new UnknownTypeException(clazz)
  }
  val makePublicationT = (makePublication _).tupled
  
  class AuthorPubTable(tag: Tag) extends Table[(Int, Int, Int)](tag, "AUTHOR_PUB") {
    def authorId = column[Int]("AUTHOR_ID")
    def pubId = column[Int]("PUB_ID")
    
    def authorKey = foreignKey("AUTHOR_FK", authorId, authorTable)(_.id)
    def pubKey = foreignKey("PUB_FK", pubId, publicationTable)(_.id)
    
    // For some publications, the author order is important.
    // This field stores the position of the author in the author list (starting from 0).
    def authorIndex = column[Int]("AUTHOR_INDEX")
    
    def * = (authorId, pubId, authorIndex)
  }
  
  val authorPubTable = TableQuery[AuthorPubTable]

  class VenueTable(tag: Tag) extends Table[(Int, String, String, Int)](tag, "VENUES") {
    def id = column[Int]("VEN_ID", O.PrimaryKey, O.AutoInc)
    def title = column[String]("TITLE")
    def editors = column[String]("EDITORS") // List of author ids, in csv format.
    def year = column[Int]("YEAR")

    def * = (id, title, editors, year)
  }

  def create() {
    db withSession { implicit session =>
      (authorTable.ddl ++ publicationTable.ddl ++ authorPubTable.ddl).create
    }
  }

  class RedundancyException[T](redundantElements: Iterable[T]) extends Exception
  class UnknownTypeException(typ: String) extends Exception {
    override def toString = super.toString + ": " + typ
  }

  // When querying the tables, we normalize the text (by, e.g., removing accents).
  private def normalize(s: String): String = {
    // Replace combined characters by normal characters + combining sequences.

    val s1 = Normalizer.normalize(s, Normalizer.Form.NFKD)
    println(s1)
    // Remove the combining sequences.
    val s2 = s1.replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    println(s2)
    // Replace multiple whitespaces with a single space.
    val s3 = s2.replaceAll("\\s+", " ")
    println(s3)
    s3.trim.toLowerCase
  }

  /**
   * Returns all of the authors that have the given last name.
   */
  private def getAuthors(s: String): List[IdPerson] = {
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
   */
  private def getAuthors(n: Name): List[IdPerson] =
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

  def publicationByTitle(s: String) = {
    val query = for {
      p <- publicationTable if p.normalizedTitle === normalize(s)
      m <- authorPubTable if m.pubId === p.id
      a <- authorTable if a.id === m.authorId
    } yield (p, a)
    db.withSession { implicit session =>
      for {
        (pubId, pubsAuthors) <- query.list() groupBy (_._1._1)
      } yield {
        val (pubs, authors) = pubsAuthors.unzip
        makePublicationT(pubs.head)(authors map makePersonT)
      }
    }
  }

  // If there is a single matching author, return a copy of it.
  // If there are no matching authors, create new one.
  // If there are multiple matching authors, throw RedundancyError
  private def getAuthor(p: Person) = {
    val as = getAuthors(p.name)
    if (as.isEmpty) makePerson(0, p.name, "") // FIXME: evaluate the right thing to do.
    else if (as.size == 1) as.head
    else throw new RedundancyException(as)
  }

  // Check whether the information in `e` is a subset of the information in `canon`.
  private def matches(e: BibtexEntry, canon: Publication): Boolean = {
    (normalize(e.title) == normalize(canon.title)
      && e.authors.forall(a => canon.authors.exists(_.name matches a)))
    // TODO: check year and journal
  }

  // Check whether we have something matching the given entry in the database;
  // if we do, return it.
  private def retrieveMatchingPubs(e: BibtexEntry): Iterable[Publication] = {
    publicationByTitle(e.title) filter ((p: Publication) => matches(e, p))
  }

  // Turns a PublicationRecord (which we assume not to be in the database, although
  // some of its authors may be) into a Publication.
  private def record2Publication(pr: PublicationRecord): Publication = {
    val authors = pr.authors map getAuthor

    // TODO: support more entry types
    if (pr.publicationType == "article-journal") {
      Article(
        id = 0,
        title = pr.title,
        authors = authors,
        year = pr.year,
        pages = pr.pages)
    } else if (pr.publicationType == "book") {
      Book(
        id = 0,
        title = pr.title,
        authors = authors,
        year = pr.year)
    } else throw new UnknownTypeException(pr.publicationType)
  }

  // Look up a bibtex entry online and turn it into a Publication.
  private def lookupEntry(e: BibtexEntry): Future[Publication] = future {
    val doi = CrossRefQuerier.query(e)
    val canonical = DOIQuerier.query(doi)
    record2Publication(canonical)
  }

  def canonicalizeBibtexEntry(e: BibtexEntry): Future[Publication] = {
    // TODO: don't lookupEntry if the type is "unpublished"
    val pubs = retrieveMatchingPubs(e)
    if (pubs.isEmpty) lookupEntry(e)
    else if (pubs.tail.isEmpty) Future.successful(pubs.head)
    else Future.failed(new RedundancyException(pubs.toSet))
  }
}

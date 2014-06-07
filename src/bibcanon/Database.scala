package bibcanon

import concurrent._
import ExecutionContext.Implicits.global
import collection.mutable.{ HashMap => MutHashMap, Set => MutSet, MultiMap }
import java.text.Normalizer
import bibtex.BibtexEntry
import bibtex.Name
import scala.slick.driver.H2Driver.simple._

/**
 * A database is a collection of authors, publications, journals, etc. with the
 * appropriate links between them and various lookup tables.
 */
class Database {
  // TODO: use a real database.
  val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver="org.h2.Driver")

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
  
  private def makePerson(id: Int, first: String, von: String, last: String, jr: String, norm: String, arxiv: String) =
    Person(id, Name(first, von, last, jr), arxiv)
  val makePersonT = (makePerson _).tupled

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
  
  private def makePublication(id: Int, clazz: String, title: String, norm: String, year: Int, venue: Int)(authors: Seq[Person]) = {
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

  class RedundancyException[T](redundantElements: Iterable[T]) extends Exception
  class UnknownTypeException(typ: String) extends Exception {
    override def toString = super.toString + ": " + typ
  }

  // When querying the tables, we normalize the text (by, e.g., removing accents).
  private def normalize(s: String) = {
    // Replace combined characters by normal characters + combining sequences.
    val s1 = Normalizer.normalize(s, Normalizer.Form.NFKD)
    // Remove the combining sequences.
    val s2 = s1.replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    // Replace multiple whitespaces with a single space.
    val s3 = s2.replaceAll("\\s+", " ")
    s3.trim.toLowerCase
  }

  def authorByName(s: String): List[Person] = {
    val tuples = for {
      a <- authorTable if a.normalizedLastName === normalize(s)
    } yield a
    db.withSession { implicit session =>
      tuples.list() map makePersonT
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

  private def matchingAuthors(n: Name) = authorByName(n.last) filter (_.name matches n)

  // If there is a single matching author, return a copy of it.
  // If there are no matching authors, create new one.
  // If there are multiple matching authors, throw RedundancyError
  private def getAuthor(n: Name) = {
    val as = matchingAuthors(n)
    if (as.isEmpty) Person(id = 0, name = n, arxivId = "")
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
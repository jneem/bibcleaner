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

trait PublicationDatabase extends Utils {this: Profile with AuthorDatabase =>
  import profile.simple._

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

  // TODO: move this to a new trait
  class VenueTable(tag: Tag) extends Table[(Int, String, String, Int)](tag, "VENUES") {
    def id = column[Int]("VEN_ID", O.PrimaryKey, O.AutoInc)
    def title = column[String]("TITLE")
    def editors = column[String]("EDITORS") // List of author ids, in csv format.
    def year = column[Int]("YEAR")

    def * = (id, title, editors, year)
  }

  // FIXME: review everything after this
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

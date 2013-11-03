package bibcanon

import concurrent._
import ExecutionContext.Implicits.global
import collection.mutable.{ HashMap => MutHashMap, Set => MutSet, MultiMap }
import java.text.Normalizer
import bibtex.BibtexEntry
import bibtex.Name
import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession

/**
 * A database is a collection of authors, publications, journals, etc. with the
 * appropriate links between them and various lookup tables.
 */
class Database {

  object AuthorTable extends Table[(Int, String, String, String, String, String)]("AUTHORS") {
    def id = column[Int]("AUTHOR_ID", O.PrimaryKey, O.AutoInc)
    def firstName = column[String]("NAME_FIRST")
    def vonName = column[String]("NAME_VON")
    def lastName = column[String]("NAME_LAST")
    def jrName = column[String]("NAME_JR")
    def arxivId = column[String]("ARVIX_ID")

    def * = id ~ firstName ~ vonName ~ lastName ~ jrName ~ arxivId
  }

  object PublicationTable extends Table[(Int, String, String, String, Int)]("PUBLICATIONS") {
    def id = column[Int]("PUB_ID", O.PrimaryKey, O.AutoInc)
    def clazz = column[String]("CLASS") // The type of publication (e.g. "article")
    def title = column[String]("TITLE")
    def authors = column[String]("AUTHORS") // List of author ids, in csv format.
    def venue = column[Int]("VENUE")

    def * = id ~ clazz ~ title ~ authors ~ venue
  }

  object VenueTable extends Table[(Int, String, String, Int)]("VENUES") {
    def id = column[Int]("VEN_ID", O.PrimaryKey, O.AutoInc)
    def title = column[String]("TITLE")
    def editors = column[String]("EDITORS") // List of author ids, in csv format.
    def year = column[Int]("YEAR")

    def * = id ~ title ~ editors ~ year
  }

  val authors: MutSet[Person] = MutSet()
  val publications: MutSet[Publication] = MutSet()
  val venues: MutSet[PublicationVenue] = MutSet()
  val series: MutSet[PublicationSeries] = MutSet()

  class RedundancyError[T](redundantElements: Iterable[T]) extends Throwable

  private val authorNameTable = new MutHashMap[String, MutSet[Person]] with MultiMap[String, Person]
  private val publicationTitleTable = new MutHashMap[String, MutSet[Publication]] with MultiMap[String, Publication]
  private val venueTitleTable = new MutHashMap[String, MutSet[PublicationVenue]] with MultiMap[String, PublicationVenue]
  private val seriesTitleTable = new MutHashMap[String, MutSet[PublicationSeries]] with MultiMap[String, PublicationSeries]

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

  def authorByName(s: String) = authorNameTable.getOrElse(normalize(s), MutSet())
  def publicationByTitle(s: String) = publicationTitleTable.getOrElse(normalize(s), MutSet())
  def venueByTitle(s: String) = venueTitleTable.getOrElse(normalize(s), MutSet())
  def seriesByTitle(s: String) = seriesTitleTable.getOrElse(normalize(s), MutSet())

  private def matchingAuthors(n: Name) = authorByName(n.last) filter (_.name matches n)

  // If there is a single matching author, return a copy of it.
  // If there are no matching authors, create new one.
  // If there are multiple matching authors, throw RedundancyError
  private def getAuthor(n: Name) = {
    val as = matchingAuthors(n)
    if (as.isEmpty) Person(name = n)
    else if (as.size == 1) as.head
    else throw new RedundancyError(as)
  }

  def addAuthor(p: Person) {
    authors.add(p)
    authorNameTable.addBinding(normalize(p.name.last), p)
  }

  def addPublication(p: Publication) {
    publications.add(p)
    publicationTitleTable.addBinding(normalize(p.title), p)
    // TODO: add authors, etc.
  }

  def addVenue(v: PublicationVenue) {
    venues.add(v)
    venueTitleTable.addBinding(normalize(v.fullName), v)
  }

  def addSeries(s: PublicationSeries) {
    series.add(s)
    seriesTitleTable.addBinding(normalize(s.title), s)
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

  // Turns a BibtexEntry (which we assume not to be in the database, although
  // some of its authors may be) into a Publication.
  private def entry2Publication(e: BibtexEntry): Publication = {
    val authors = e.authors map getAuthor

    // TODO: support more entry types
    // TODO: consider turning BibtexEntry.entryType into an enum
    if (e.entryType == "article") {
      Article(
        title = e.title,
        authors = authors,
        year = Some(e.year))
    } else throw new NotImplementedError("entry2Publication entryTypes")
  }

  // Look up a bibtex entry online and turn it into a Publication.
  private def lookupEntry(e: BibtexEntry): Future[Publication] = future {
    val doi = CrossRefQuerier.query(e)
    val canonical = DOIQuerier.query(doi)
    entry2Publication(canonical)
  }

  def canonicalizeBibtexEntry(e: BibtexEntry): Future[Publication] = {
    val pubs = retrieveMatchingPubs(e)
    if (pubs.isEmpty) lookupEntry(e)
    else if (pubs.tail.isEmpty) Future.successful(pubs.head)
    else Future.failed(new RedundancyError(pubs.toSet))
  }
}
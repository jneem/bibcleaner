package bibcanon

import concurrent._
import ExecutionContext.Implicits.global
import collection.mutable.{HashMap => MutHashMap, Set => MutSet, MultiMap}
import java.text.Normalizer
import bibtex.BibtexEntry

/**
 * A database is a collection of authors, publications, journals, etc. with the
 * appropriate links between them and various lookup tables.
 */
class Database {
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
    publicationByTitle(e.title) filter ((p:Publication) => matches(e, p))
  }
  
  // TODO
  private def entry2Publication(e: BibtexEntry): Publication = ???
  
  // Look up a bibtex entry online and turn it into a Publication.
  private def lookupEntry(e: BibtexEntry): Future[Publication] = future {
    val doi = CrossRefQuerier.query(e)
    val canonical = DOIQuerier.query(doi)
    entry2Publication(canonical)
  }
  
  def canonicalizeBibtexEntry(e: BibtexEntry): Future[Publication] = {
    retrieveMatchingPubs(e) match {
      case List() => Future.failed(new NotImplementedError("no cached candidates"))
      case List(p) => Future.successful(p)
      case ps => Future.failed(new RedundancyError(ps.toSet))
    }
  }
}
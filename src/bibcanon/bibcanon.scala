package bibcanon

import collection.mutable.{ Set => MutSet }
import collection.immutable.Range
import bibtex.Name

/**
 * A person has a name, and possibly some associated metadata.
 */
trait Person {
  def name: Name
  def arXivId: Option[String]

  def toBibtex = name.toBibtex
}

object Person {
  def apply(nameVal: Name, aId: String="") = new Person {
    def name = nameVal
    val arXivId = if (aId == "") None else Some(aId)
  }
}


/**
 * An identified person.
 *
 * Besides having a name and whatever other metadata, this person
 * also has a unique internal id. Instances of this trait should
 * not be instantiated manually, but only by Database (which is in
 * a position to ensure that everyone has a unique id).
 */
trait IdPerson extends Person {
  def id: Int
}

/**
 * A record describing a publication.
 * 
 * This is different from a BibtexEntry, because BibtexEntry is tied
 * to particular textual representations (e.g., of author names).
 * 
 * This is not a semantic structure, in the sense that we only
 * give author and journal names, and not references to author
 * and journal objects.
 */
trait PublicationRecord {
  // Mandatory fields
  def title: String
  def authors: Seq[Person]
  def publicationType: String
  
  // Optional fields
  def year: Option[Int]
  
  /**
   * Journal name, for example.
   */
  def containerTitle: Option[String]
  
  def pages: Option[Range]
  def issue: Option[Int]
  def volume: Option[Int]
  def publisher: Option[String]
  def doi: Option[String]
}

trait Publication extends PublicationRecord {
  def id: Int
  def authors: Seq[IdPerson]

  def containerTitle: Option[String] = None
  def pages: Option[Range] = None
  def issue: Option[Int] = None
  def volume: Option[Int] = None
  def publisher: Option[String] = None
  def doi: Option[String] = None

  def toBibtex(key: String): String
}

case class Article(
  id: Int,
  title: String,
  authors: Seq[IdPerson],
  year: Option[Int] = None,
  venue: Option[PublicationVenue] = None,
  override val pages: Option[Range] = None) extends Publication {

  def publicationType = "article"

  // TODO: unescape things properly
  // TODO: handle corner cases like empty authors
  override def toBibtex(key: String) = {
    val auth = authors map (_.toBibtex) mkString " and "
    "@article{" + key + ",\n" + s"title = {$title},\n" + s"authors = {$auth},\n" + "}\n"
  }
}

case class Book(
  id: Int,
  title: String,
  authors: Seq[IdPerson],
  year: Option[Int] = None,
  override val publisher: Option[String] = None
  ) extends Publication {

  def publicationType = "book"
  
  // TODO: unify avoid code duplication
  override def toBibtex(key: String) = {
    val auth = authors map (_.toBibtex) mkString " and "
    "@book{" + key + ",\n" + s"title = {$title},\n" + s"authors = {$auth},\n" + "}\n"
  }
}

/**
 * Some papers are published in slightly different forms in multiple venues
 * (e.g. a conference version and a journal version). We try to detect this and
 * build links between the different versions.
 * (TODO)
 */

object Publication {
}

/**
 * A PublicationVenue is a place the publications appear.
 *
 * This could be a website (e.g. arXiv), conference proceedings, or a
 * journal issue.
 */
abstract class PublicationVenue {
  val editors: MutSet[Person] = MutSet()
  val publisher: String = ""
  val year: Int = 0

  /// For example, "Proceedings of the 45th Symposium on the Theory of Computing".
  def fullName: String
  /// For example, "Proc. 45th STOC"
  def shortName: String
}

class Proceedings(val conference: Conference) extends PublicationVenue {
  var number: Option[Int] = None

  private def ordinal(n: Int) = {
    val endings = Array("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
    if (10 <= n % 100 && n % 100 <= 20) n.toString + "th"
    else n.toString + endings(n % 10)
  }

  def fullName = number match {
    case None => "Proceedings of the " + conference.title
    case Some(n) => "Proceedings of the " + ordinal(n) + " " + conference.shortTitle
  }

  def shortName = number match {
    case None => "Proc. " + conference.shortTitle
    case Some(n) => "Proc. " + ordinal(n) + " " + conference.shortTitle
  }
}

class JournalIssue(val journal: Journal) extends PublicationVenue {
  var issue: Int = 0
  val volume: Int = 0

  def fullName = journal.title
  def shortName = journal.shortTitle
}

/**
 * A PublicationSeries is, for example, a journal or a conference.
 */
class PublicationSeries {
  var title = ""
  var shortTitle = ""
}

class Conference extends PublicationSeries {
}

class Journal extends PublicationSeries {
}

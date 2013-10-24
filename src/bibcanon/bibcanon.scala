package bibcanon

import collection.mutable.{Set => MutSet}
import bibtex.Name

class Person {
  var name = Name("", "")
  
  /// The person's ZentralBlatt id.
  var zbId: Option[String] = None
  
  /// The person's arXiv author id.
  var arxivId: Option[String] = None
  
  val publications: MutSet[Publication] = MutSet()
}

class Publication {
  var title: String = ""
  val authors: MutSet[Person] = MutSet()
  var year: Int = 0
  var venue: Option[PublicationVenue] = None
  var pages: Option[(Int, Int)] = None
  
  /**
   * Some papers are published in slightly different forms in multiple venues
   * (e.g. a conference version and a journal version). We try to detect this and
   * build links between the different versions.
   * (TODO)
   */
}

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
  var publisher: String = ""
  var year: Int = 0

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

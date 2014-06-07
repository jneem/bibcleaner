package bibcanon

import bibtex.Name

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
  def authors: List[Name]
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
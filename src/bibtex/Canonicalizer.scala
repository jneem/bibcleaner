package bibtex

import collection.mutable
import scala.Predef.Set.apply
import scala.collection.immutable.List.apply

case class Name(first: String, von: String, last: String, jr: String) {
  // TODO: handle middle initial: "Peter L" -> "P. L."
  private def initial(n: String): String =
    if (n.isEmpty) "" else (n(0) + ".")
  def full = List(first, von, last, jr).filter(_ != "").mkString(" ")
  def short = List(initial(first), von, last, jr).filter(_ != "").mkString(" ")
  def sanitized = Name(first.filter(_.isLetter).capitalize,
                       von.filter(_.isLetter).capitalize,
                       last.filter(_.isLetter).capitalize,
                       jr.filter(_.isLetter).capitalize)
}

case class Author(name: Name, url: String)

// A class for finding the "canonical version" of a Name,
// using some sort of fuzzy matching.
// TODO: make Peter L. Bartlett match Peter Bartlett
class Canonicalizer {
  // This maps either a first name or a last name to the set of all matching Names.
  private val mapping = new mutable.HashMap[String, mutable.Set[Name]] with mutable.MultiMap[String, Name]

  def += (n: Name) = {
    val m = n.sanitized
    mapping.addBinding(m.first, n)
    mapping.addBinding(m.last, n)
  }

  // TODO: better logging
  def apply(n: Name): Name = {
    val candidates = mapping.getOrElse(n.first, Set()) ++ mapping.getOrElse(n.last, Set())
    if (candidates.contains(n)) n
    else {
      val matchingCandidates = candidates.filter(almostMatches(_, n))
      if (matchingCandidates.isEmpty) {
        System.err.println("No match")
        n
      } else if (matchingCandidates.size > 1) {
        System.err.println("Multiple matches")
        matchingCandidates.head
      } else {
        matchingCandidates.head
      }
    }
  }

  // TODO: allow \'E to match E, etc.
  private def almostMatches(n1: Name, n2: Name): Boolean = {
    val m1 = n1.sanitized
    val m2 = n2.sanitized
    def absentOrPrefix(s1: String, s2: String) = s1.isEmpty || s2.isEmpty || s1.startsWith(s2) || s2.startsWith(s1)

    // Last names must match
    ((n1.last == n2.last) &&
    // If the other parts are present, they must match (or one must be a prefix of the other)
      absentOrPrefix(m1.first, m2.first) &&
      absentOrPrefix(m1.von, m2.von) &&
      absentOrPrefix(m1.jr, m2.jr))
  }

}

object Canonicalizer {
  def readFromFile(fn: String): Canonicalizer = {
    val root = xml.XML.loadFile(fn)
    val ap = new AuthorParser
    val ret = new Canonicalizer
    (root \ "author").foreach(p => {
      val name = (p \ "name").head.text
      // TODO: better error handling
      ret += ap.authorName(new util.parsing.input.CharSequenceReader(name)).get
    })

    ret
  }

  // TODO: put this somewhere else...
  def urlMap(fn: String): Map[Name, String] = {
    val root = xml.XML.loadFile(fn)
    val ap = new AuthorParser
    val mapping: Iterable[(Name, String)] = (root \ "author").map(p => {
      val nameStr = (p \ "name").head.text
      // TODO: better error handling
      val name: Name = ap.authorName(new util.parsing.input.CharSequenceReader(nameStr)).get
      (name, (p \ "url").head.text)
    })

    mapping.toMap
  }
}

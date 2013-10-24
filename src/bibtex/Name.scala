package bibtex

import java.util.regex.Pattern

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

  /**
   * Checks if one name is an abbreviation of another.
   * 
   * Two names match if their family names are equal, and for all the
   * other parts of the name, one is an abbreviation of the other.
   * 
   * For example, "P L Bartlett" matches "Peter L Bartlett" or "Peter Bartlett", but not "L Bartlett" 
   */
  def matches(other: Name): Boolean = {
    // Checks if s1 is an abbreviation of s2.
    def abbrev(s1: String, s2: String) = {
      val nonLetter = Pattern.compile("\\P{L}+")

      // Split "Peter L" into List("Peter", "L"), and drop non-letters
      // (so that "L." is equivalent to "L")
      def splitName(s: String): List[String] = (nonLetter split s map (_.toLowerCase) filter (_ != "")).toList
      
      // Two parts of a name (e.g, "Peter" and "P") match if they are equal,
      // or if one is an initial of the other.
      def stringsMatch(x: String, y: String) = (x == y) || (x.length == 1 && x(0) == y(0)) || (y.length == 1 && y(0) == x(0))

      def splitsMatch(xs: List[String], ys: List[String]): Boolean = (xs, ys) match {
        case (x :: xs1, y :: ys1) => stringsMatch(x, y) && splitsMatch(xs1, ys1)
        case _ => true
      }
      
      splitsMatch(splitName(s1), splitName(s2))
    }
    last == other.last && abbrev(first, other.first) && abbrev(von, other.von) && abbrev(jr, other.jr)
  }
}

object Name {
  def apply(first: String, last: String) = new Name(first, "", last, "")
}


package bibcanon

import java.util.regex.Pattern

class Name(val given: String, val family: String, val articulated: String, val honorific: String) {
  def longName: String = List(given, articulated, family, honorific).mkString(" ")
  
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
    family == other.family && abbrev(given, other.given) && abbrev(articulated, other.articulated) && abbrev(honorific, other.honorific)
  }
}

object Name {
  def apply() = new Name("", "", "", "")
  def apply(g: String, f: String, a: String, h: String) = new Name(g, f, a, h)
}
package bibtex

// A BibtexEntry is essentially a collection of name/value pairs,
// along with some methods to help in sanitizing the values.
trait BibtexEntry extends Map[String, String] {
  def entryType: String
  
  /**
   * The title field.
   * 
   * The title will be returned in a format suitable for displaying in plain text.
   * All braces will be removed, and all words except the first will be
   * turned to lower case (except for words protected by braces).
   */
  def title: String = get("title") match {
    case Some(t) => {
      val parsedTitle = EntryParser.parse(io.Source.fromString(t))
      // Turn pseudochars to strings by stripping any braces and backslashes.
      // For pseudochars that are not braced, we also turn all characters to
      // lower case.
      def pcToString(pc: PseudoChar) = {
        val ret = pc.toString.replaceAll("[{}\\\\]", "")
        if (pc.braced) ret else ret.toLowerCase()
      }

      (parsedTitle map pcToString).mkString.capitalize
    }
    case None => ""
  }

  // TODO: change some of these to Options.
  // TODO: canonicalize journals also
  def journal: String = getOrElse("journal", "")

  def authors: List[Name] = get("author") match {
    case Some(a) => AuthorParser.parse(io.Source.fromString(a))
    case None => List()
  }

  def year: Int = get("year") match {
    case Some(y) => try y.toInt catch { case (ex: NumberFormatException) => 0 }
    case None => 0
  }
}
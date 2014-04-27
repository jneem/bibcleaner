package bibtex

import collection.immutable.HashMap

// A BibtexEntry is essentially a collection of name/value pairs,
// along with some methods to help in sanitizing the values.
class BibtexEntry(val entryType: String, val key: String, props: Map[String, String])
  extends Map[String, String] {

  override def get(key: String) = props.get(key)
  override def iterator = props.iterator
  override def -(key: String) = props - key
  override def +[A >: String](kv: (String, A)) = props + kv

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

object BibtexEntry {
  def apply(entryType: String, key: String, props: Iterable[(String, String)]) = {
    // Make the keys all lowercase, since the keys are case insensitive.
    val normalizedProps = props map (x => (x._1.toLowerCase, x._2))
    new BibtexEntry(entryType.toLowerCase, key, new HashMap ++ normalizedProps)
  }
}

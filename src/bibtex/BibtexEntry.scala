package bibtex

import collection.mutable
import Name.apply
import scala.Predef.Map.apply

// A BibtexEntry is essentially a collection of name/value pairs,
// along with some methods to help in sanitizing the values.
case class BibtexEntry(entryType: String, key: String, initProps: Iterable[(String, String)])
  extends mutable.HashMap[String,String] {
  // Make the keys all lowercase, since the keys are case insensitive.
  this ++= initProps map (x => (x._1.toLowerCase, x._2))

  // TODO: figure out a better interface
  var canon : Option[Canonicalizer] = None
  var urlResolver : Map[Name, String] = Map()

  def title: String = if (contains("title")) this("title") else ""
  def link: String = {
    if (contains("url")) this("url")
    else if (contains("archivePrefix")
             && this("archivePrefix") == "arXiv"
             && contains("eprint")) {
      "http://arxiv.org/abs/" + this("eprint")
    } else {
      ""
    }
  }

  def authors: List[String] = {
    /*val ap = new AuthorParser
    val reader = new util.parsing.input.CharSequenceReader(this("author"))
    val names = ap.authorNameList(reader).get
    def makeCanon(n: Name) = canon match {
      case Some(c) => c(n)
      case None => n
    }
    def htmlName(n: Name) = {
      val nameStr = n.short.replaceAll(" ", "&nbsp;")
      if (urlResolver.contains(n)) {
        """<a href="%s">%s</a>""".format(urlResolver(n), nameStr)
      } else {
        nameStr
      }
    }
    // println(names)
    // filter myself out of the list of names
    names.filter(_ != Name("Joe", "", "Neeman", "")).map(makeCanon).map(htmlName)
    */
    List()
  }

  def year: String = this("year")

  // TODO: canonicalize journals also
  def journal: String = getOrElse("journal", getOrElse("note", ""))

  // Writes out this bibtex element as a table row.
  def toHtml(id: String): String = {
    val titleTd = ("""<td><a class="bibtitle" href="%s">%s</a>""".format(link, title)
        + """<br><span class="bibjournal"> %s, %s</span></td>""".format(journal, year))

    val authorsTd = """<td class="bibauthor">%s</td>""".format(authors.mkString("<br>"))

    """<tr class="%s">""".format(id) + titleTd + authorsTd + "</tr>"
  }
}


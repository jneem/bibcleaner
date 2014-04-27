package bibcanon

import bibtex.{ Name, TexChars }
import java.nio.charset.{Charset, CharsetEncoder}
import java.util.regex.Pattern

object BibtexFormatter {
  val ws = Pattern.compile("\\s")

  def author2Bibtex(n: Name) = {
    // There are three possible formats for a name in BibTeX, but we only
    // use two, since the third is redundant. If a name has a "jr" part,
    // we write
    // von Last, First, jr
    // If it has no "jr" part, we simply omit the second comma.

    // The only complicated part is encoding "von Last", because there is
    // no explicit way to delimit them. The BibTex rule is that the "von"
    // part is the longest sequence of words whose last word starts
    // with a lower case. This means that if we have lower-case words that
    // we want to include in "Last," then we can do so by making them uncased --
    // i.e. by enclosing their first letter in braces. On the other hand,
    // if we want "von" to end with a non-lower-case word then we're stuck.
    

    // Make any lower-cased words uncased.
    def uncase(s: String) = {
      if (!s.isEmpty && s(0).isLower) "{" + s + "}"
      else s
    }
    
    // Make sure no words in the given name are "and." We replace any "and" with "a{nd}",
    // which will have the same effect, but won't be recognized by BibTex as a name separator.
    def escapeOneAnd(w: String) = if (w == "and") "a{nd}" else w
    def escapeAnd(s: String) = {
      ws.split(s) map escapeOneAnd mkString " "
    }
    
    val lasts = ws.split(n.last) map escapeOneAnd map uncase
    if (lasts.isEmpty) {
      println("warning: last name should be non-empty")
    }
    val last = lasts mkString " "
    
    if (!n.von.isEmpty) {
      val lastVon = ws.split(n.von).last
      if (!lastVon.isEmpty && !lastVon(0).isLower)
        println("warning: last word in von is not lower case: bibtex will be unhappy")
    }
    
    val von = escapeAnd(n.von)
    val first = escapeAnd(n.first)
    val jr = if (!n.jr.trim.isEmpty) escapeAnd(n.jr) + ", " else ""
    s"$von $last, $jr$first".trim
  }
  
  def authors2Bibtex(ns: List[Name]) = (ns map author2Bibtex) mkString " and "
}
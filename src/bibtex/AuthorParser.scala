package bibtex

import scala.collection.Seq
import scala.collection.mutable.Map
import org.parboiled.scala._

sealed trait Case
case object Upper extends Case
case object Lower extends Case
case object Uncased extends Case
  
class Word(chars: List[PseudoChar]) {
  // The case of a word is the case of its first character that has
  // a case. If no characters have case, the word is uncased.
  def wordCase: Case = {
    chars.find(_.hasCase) match {
      case Some(c) => c.charCase
      case None => Uncased
    }
  }
  
  def upperCase: Boolean = wordCase == Upper
  def lowerCase: Boolean = wordCase == Lower
  def noCase: Boolean = wordCase == Uncased
  
  def hasCase: Boolean = this.wordCase != Uncased
  
  override def toString: String = chars.mkString
  
  // In converting to a plain string, we throw away braces and backslashes.
  def toPlainString: String = toString.filterNot("{}\\".contains(_))
}

class AuthorParser extends EntryParser {
  // Note that bibtex treats a tilde as white-space for the purposes
  // of separating names.
  // TODO: hyphens should be also treated as whitespace for the purposes
  // of separating names, but they shouldn't be discarded.
  private val whitespaceChars = " \t\n\u000B\f\r~"
  def ws: Rule0 = zeroOrMore(anyOf(whitespaceChars))
  
  // A word is a sequence of pseudochars, with commas forbidden. The word "and" is reserved, so we don't match it.
  def word: Rule1[Word] = (oneOrMore(pseudoChar("," + whitespaceChars)) ~? (_ != "and") ~ ws) ~~> (new Word(_)) 
  
  def wordsToName(ws: List[Word]): String = ws.map(_.toPlainString).mkString(" ")
  
  // There are three possible formats for names:
  // - First von Last
  // - von Last, First
  // - von Last, Jr, First
  
  // The "First von Last" case is the only one that takes effort to parse.
  // (The other two are just determined by comma placement)
  val firstVonLastFn: List[Word] => Name = ws => {
    val len = ws.length
    val firstVon = ws.indexWhere(_.lowerCase)
    val lastVon = ws.lastIndexWhere(_.lowerCase, len-2)
    
    def str(from: Int, until: Int): String = {
      wordsToName(ws.slice(from, until))
    }
    
    if (firstVon == -1 || firstVon == len-1) {
      // If there is no von, then the last word is the last name, and everything
      // else is the first name. Recall that there must be a last name, so even the
      // von looks like it should start on the last word, we just leave out the von.
      Name(str(0, len-1), "", str(len-1, len), "")
    } else {
      Name(str(0, firstVon), str(firstVon, lastVon+1), str(lastVon+1, len), "")
    }
  }
  
  val vonLastFirstFn: (List[Word], List[Word]) => Name = (vonLast, first) => {
    vonLastJrFirstFn(vonLast, List(), first)
  }
  
  val vonLastJrFirstFn: (List[Word], List[Word], List[Word]) => Name =
    (vonLast, jr, first) => {
      def str(from: Int, until: Int): String = {
        wordsToName(vonLast.slice(from, until))
      }
      val len = vonLast.length
      val lastVon = vonLast.lastIndexWhere(_.lowerCase, len-2)
      if (lastVon == -1) {
        Name(wordsToName(first), "", wordsToName(vonLast), wordsToName(jr))
      } else {
        Name(wordsToName(first), str(0, lastVon+1), str(lastVon+1, len), wordsToName(jr))
      }
    }
  
  def comma: Rule0 = "," ~ ws
  def firstVonLast: Rule1[Name] = rule { oneOrMore(word) ~~> firstVonLastFn }
  def vonLastFirst: Rule1[Name] = rule {
    (zeroOrMore(word) ~ comma ~ zeroOrMore(word)) ~~> vonLastFirstFn
  }
  def vonLastJrFirst: Rule1[Name] = rule {
    (zeroOrMore(word) ~ comma ~ zeroOrMore(word) ~ comma ~ zeroOrMore(word)) ~~>
    vonLastJrFirstFn
  }
  
  def name: Rule1[Name] = vonLastJrFirst | vonLastFirst | firstVonLast
  
  def nameList: Rule1[List[Name]] = rule {
    val and: Rule0 = "and" ~ ws
    
    // To be more robust, we also allow malformed names.
    val errfn = (str: String) => println(s"Could not parse name: $str")

    val maybeName: Rule1[Option[Name]] = rule {
      name ~~> (Some(_)) |
      zeroOrMore(word | keep(comma)) ~> errfn ~~> ((_,_) => None)
    }
    (maybeName ~ zeroOrMore(and ~ maybeName)) ~~> ((x, y) => (x::y).flatten)
  }
}

object AuthorParser {
  def parse(input: io.Source): List[Name] = {
    val parser = new AuthorParser
    val runner = ReportingParseRunner(parser.nameList)
    runner.run(input).result.get
  }
}
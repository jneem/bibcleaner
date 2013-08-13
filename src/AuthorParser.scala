package bibtex

import util.parsing.combinator._
import util.parsing.input.CharSequenceReader
import collection.immutable.StringOps
//import bibtex.BibtexParser

// The specification is mainly taken from here:
// http://maverick.inria.fr/Membres/Xavier.Decoret/resources/xdkbibtex/bibtex_summary.html#splitting_examples
// However, note that a comma does not count as whitespace.

// Parser for the author field of a bibtex entry.
class AuthorParser extends BibtexParser {
  // TODO: figure out how tex-encoded accents are treated
  // FIXME: use character classes to include all characters, without
  // hacking in ℓ
  val chars = """ℓA-Za-z0-9\\"'`"""
  def pseudoChar: Parser[String] = bracedString | ("[" + chars + "]").r

  // TODO: I'm not sure how much I trust the webpage above, when it
  // says that all non-alnum characters are treated as whitespace...
  def whitespace: Parser[String] = ("[^" + chars + "{},]").r

  override def skipWhitespace = false

  def anyWord: Parser[List[String]] =
    (whitespace?) ~> (pseudoChar+) <~ (whitespace?)

  // The word "and" by itself (case sensitive!) is treated specially, so
  // it doesn't count as a word.
  def word = new Parser[List[String]] {
    def apply(in: Input) = anyWord(in) match {
      case Success(result, next) if result == List("a", "n", "d") => Failure("\"and\" is not a word", in)
      case s@Success(result, next) => s
      case f@Failure(msg, next) => f
    }
  }

  sealed trait WordCase
  case object UpperCase extends WordCase
  case object LowerCase extends WordCase
  case object NoCase extends WordCase

  def wordCase(word: List[String]): WordCase = {
    // pseudo-characters that are letters have the appropriate case
    // pseudo-characters that are digits are considered lower-case
    // all other pseudo-characters (eg. balanced brace expressions) have no case.

    // Note: need to explicitly coerce to StringOps, because there is also an implicit
    // conversion from String to Parser[String]
    val firstCasedLetter = word.find(char => char.length == 1 && (char:StringOps)(0).isLetterOrDigit)
    val letterCase = firstCasedLetter.map(char => if ((char:StringOps)(0).isUpper) UpperCase else LowerCase)
    letterCase.getOrElse(NoCase)
  }

  def casedWord(targetCase: WordCase) = new Parser[List[String]] {
    def apply(in: Input): ParseResult[List[String]] = word(in) match {
        case Success(result, next) if wordCase(result) == targetCase => Success(result, next)
        case Success(result, next) => Failure("word did not have case " + targetCase.toString, in)
        case f@Failure(msg, next) => f
    }
  }

  type Word = List[String]
  type PName = (List[Word], List[Word], List[Word], List[Word]) // (First, von, Last, Jr.)

  def upperCaseWord: Parser[Word] = casedWord(UpperCase)
  def lowerCaseWord: Parser[Word] = casedWord(LowerCase)
  def uncasedWord: Parser[Word] = casedWord(NoCase)
  def notUpperCaseWord: Parser[Word] = lowerCaseWord | uncasedWord

  // A sequence of words that is not everything.
  // (Allowed to be empty)
  def seqOf(p: Parser[Word]): Parser[List[Word]] = (
    (guard(word ~ word) ~> p ~ seqOf(p)) ^^ { case w ~ ws => w :: ws }
    | success(List())
  )

  // The longest sequence of words -- matching p and ending with end --
  // that is not the whole sequence. (Allowed to be empty)
  def seqEndingWith(p: Parser[Word], end: Parser[Word]): Parser[List[Word]] = {
    // Some sequence, but not the longest:
    lazy val someSeq: Parser[List[Word]] = guard(word ~ word) ~> (
      (p ~ someSeq) ^^ { case w ~ ws => w :: ws }
      | end ^^ (List(_))
    )

    // Join them together to get the longest:
    lazy val longestSeq: Parser[List[Word]] = (
      (someSeq ~ longestSeq) ^^ { case ws ~ vs => ws ++ vs }
      | success(List())
    )

    longestSeq
  }

  // There are 2 possible naming formats:
  // 1) First von Last:
  def firstVonLast: Parser[PName] = {
    // First is the longest sequence of upper case words that is not everything.
    val first = seqOf(upperCaseWord)
    // von is the longest sequence of words such that the last one
    // is lower case.
    val von = seqEndingWith(word, lowerCaseWord)
    // Last is everything else.
    val last = word+

    first ~ (von?) ~ last ^^ {
      case first ~ von ~ last => (first, von.getOrElse(List()), last, List())
    }
  }

  // 2) von Last, (Jr,)? First
  def vonLastJrFirst: Parser[PName] = {
    // von is the longest sequence of words whose last name is not upper-case.
    val von = seqEndingWith(word, notUpperCaseWord)
    // Last is everything else up to the first comma.
    val last = (word+) <~ ",".r
    // Jr is everything up to the second comma (if there is one).
    val jr = (word*) <~ ",".r
    // First is everything else.
    val first = word*

    (von?) ~ last ~ (jr?) ~ first ^^ {
      case von ~ last ~ jr ~ first => (first, von.getOrElse(List()), last, jr.getOrElse(List()))
    }
  }

  // TODO: handle TeX encoding and braces.
  def authorName: Parser[Name] = (vonLastJrFirst | firstVonLast) ^^ { case (first, von, last, jr) => {
      def transformName(name: List[Word]) = name.map(_.mkString).mkString(" ")
      Name(transformName(first), transformName(von), transformName(last), transformName(jr))
    }
  }

  def authorNameList: Parser[List[Name]] =
    ((authorName ~ "and".r ~ authorNameList ) ^^ { case n ~ _ ~ ns => n :: ns }) |
    (authorName ^^ (List(_)))
}

object AuthorParser {
    // Removes all braces from a string. Any text that is not surrounded
    // by braces is turned to lower-case.
  def unbraceString(s: String) = {
    val authorParser = new AuthorParser
    val input = new CharSequenceReader(s)

    // If a pseudocharacter is
    // a braced string, remove the braces.  If it is a simple character,
    // turn it to lowercase.
    def processChar(c: String) =
      if (c(0) == '{') c.replaceAll("[{}]", "") else c.toLowerCase

    def processWord(w: List[String]) = {
      val result = w.map(processChar).mkString
      result
    }

    val parsed = (authorParser.anyWord+)(input).get
    val processed = parsed.map(processWord)

    processed.mkString(" ").capitalize
  }
}


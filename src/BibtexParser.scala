package bibtex

import scala.util.parsing.combinator._
import scala.util.matching.Regex

/**
 * Parses a bibtex file into a list of entries.
 * Does not parse the fields of the entries.
 */
class BibtexParser extends RegexParsers {

  // Creates a parser that matches a nested delimited string.
  // char matches a single character of the string. It should not match either
  // delimiter, and it should not match the empty string.
  def delimitedString(leftDelim: Parser[String],
		              char: Parser[String],
		              rightDelim: Parser[String]): Parser[String] = {
    val beginning = leftDelim
    val middle = (log(char)("onechar")*) ^^ (_.mkString)
    println(middle)
    def end: Parser[String] = (
        log(rightDelim)("right")
        | (log("\\z".r)("eof") ~> err("Unterminated string."))
        | (log(".".r ~! middle ~! end)("illegal") ~> err("Illegal char in string."))
    )
    (log(beginning)("begin") ~! log(middle)("mid") ~! log(end)("end")) ^^ {case a ~ b ~ c => a + b + c}
  }
 
  // A basic braced string cannot contain an @ in it:
  // { foo bar } is allowed, but
  // { foo @ bar } is not allowed.
  def bracedString: Parser[String] = delimitedString("\\{".r,
	                                                 ("""[^{}@]+""".r | bracedString),
	                                                 "\\}".r)
	                                             
  // Within quotes, a braced string can contain an @:
  // "foo {@bar}" is allowed.  This parser just matches the {@bar} part.
  def bracedAtString: Parser[String] = delimitedString("\\{".r,
                                                       ("""[^{}]+""".r | bracedAtString),
                                                       "\\}".r)

  // Quotes may not be nested inside a quoted string. Braced strings may be nested
  // (although braces must match) and they may contain @.
  def quotedString: Parser[String] = delimitedString("\"".r,
                                                     ("""[^{}"]+""".r | bracedAtString),
                                                     "\"".r)
  
  // Matches almost anything containing balanced quotes and braces.
  // This is useful for recovering from errors.
  // forbidden is a string containing characters that are forbidden
  // (unless they appear in a quoted or braced string)
  // TODO: make the value of this a more readable representation of what was parsed.
  def garbage(forbidden: String): Parser[String] = {
    val bare_char = ("[^" + forbidden + "{}\"]").r
    ((quotedString | bracedString | bare_char)*) ^^ (_.mkString(" "))
  }
  
  // Apply the given parser. If it fails, consume input until the next forbidden
  // character and log a parse error.
  def withError[T](name: String, forbidden: String, parser: Parser[T]): Parser[T] = {
    def garbageError(result: ParseResult[String]): ParseResult[T] =
      Error(s"tried to parse $name but found ${result.get}", result.next)
      
    parser | Parser(garbage(forbidden) andThen garbageError)
  }

  def name: Parser[String] = """[A-Za-z_]+""".r

  // Strip out the outermost braces/quotes in a bracedString or quotedString,
  // but leave any nested braces.
  def value: Parser[String] = {
    (bracedString ^^ (_.dropRight(1).drop(1))) |
    (quotedString ^^ (_.dropRight(1).drop(1))) |
    "[0-9]+".r
  }

  // A field is a "name = value" pair.
  def field: Parser[(String, String)] = {
    val fieldName = withError("field-name", "=", name ^^ (_ toLowerCase))
    val fieldValue = withError("field-value", ",", value)
    (fieldName <~ "=".r) ~ fieldValue ^^ {case a ~ b => (a, b)}
  }

  // A sequence of fields, separated by commas and (optionally)
  // terminated by one, also.
  def fieldList: Parser[List[(String, String)]] = {
    val oneField = withError("field", ",", field)
    repsep(oneField, ",".r) <~ (",".r?)
  }

  // TODO: I'm not sure which characters are actually allowed in the key.
  def key: Parser[String] = """[-A-Za-z0-9:_]+""".r

  // An entry begins with "@name { key,".
  // It is followed by a list of fields, and it terminates with "}".
  // Instead of '{}', the entry can also be surrounded by '()'
  def entry: Parser[BibtexEntry] =
    (("@".r ~! name ~! "\\{".r ~! key ~! ",".r ~! fieldList ~! "\\}".r)
    |("@".r ~! name ~! "\\(".r ~! key ~! ",".r ~! fieldList ~! "\\)".r)) ^^ {
    case _ ~ n ~ _ ~ k ~ _ ~ fields ~ _ => new BibtexEntry(n.toLowerCase(), k, fields)
  }

  // TODO: support @comment.
  // Anything that occurs outside of an @entry{} block is a comment.
  def comment: Parser[String] = "[^@]+".r

  // An entire bibtex file.
  def file: Parser[List[BibtexEntry]] = {
    def maybeEntry:Parser[Option[BibtexEntry]] =
      (entry ^^ (Some(_))) | (comment ^^ (_ => None))

    phrase((maybeEntry*) ^^ (_.flatten))
  }
}


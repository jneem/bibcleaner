package bibtex

import org.parboiled.scala._

class BibtexParser extends Parser {
  // keep("a") makes a rule that matches a and pushes it onto the value stack.
  def keep(r: Rule0): Rule1[String] = r ~> identity

  /**
   * Creates a rule to match a delimited string.
   *
   * This will match strings which start with `leftDelim`, then contain some
   * characters matching `char`, and terminate with `rightDelim`.
   */
  def delimitedString(leftDelim: String, char: => Rule0, rightDelim: String): Rule1[String] = {
    leftDelim ~ keep(zeroOrMore(char)) ~ rightDelim
  }

  /**
   * This is a convenience overload for `delimitedString`,
   * which is particularly useful for strings containing nested substrings.
   */
  def delimitedString(leftDelim: String,
    forbiddenChars: String,
    subString: => Rule0,
    rightDelim: String): Rule1[String] = {
    delimitedString(leftDelim, (oneOrMore(noneOf(forbiddenChars)) | subString), rightDelim)
  }

  /**
   * A braced string may contain arbitrarily nested braced strings, but cannot contain `@`.
   */
  def bracedString: Rule1[String] = rule { delimitedString("{", "{}@", bracedString0, "}") }
  
  /**
   * The same as braced string, but without pushing a value.
   */
  def bracedString0: Rule0 = bracedString ~~% (_ => ())

  /**
   * This is a nested braced string that may contain `@`. It does not appear by itself in bibtex,
   * but it appears as a substring of a quoted string.
   */
  def bracedAtString: Rule1[String] = rule { delimitedString("{", "{}", bracedAtString0, "}") }
  
  /**
   * The same as bracedAtString, but without pushing a value.
   */
  def bracedAtString0: Rule0 = bracedAtString ~~% (_ => ())

  /**
   * A quoted string may contain nested braced strings (with `@`) but may not contain
   * nested quoted strings.
   */
  def quotedString: Rule1[String] = rule { delimitedString("\"", "\"{}", bracedAtString0, "\"") }

  /**
   * Matches a legal name for either a block or an entry.
   */
  def name: Rule0 = oneOrMore(("A" - "Z") | ("a" - "z") | "_")

  def number: Rule0 = oneOrMore("0" - "9")

  /**
   * Matches the right hand side of a "name = value" pair.
   */
  def value: Rule1[String] = rule { quotedString | bracedString | keep(number) }

  def WS: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  def comma: Rule0 = rule { "," ~ WS }

  /**
   * Matches a "name = value" pair.
   */
  def nameValue: Rule1[(String, String)] = rule {
    (keep(name) ~ WS ~ "=" ~ WS ~ value ~ WS) ~~> (_ -> _)
  }

  def nameValueList: Rule1[List[(String, String)]] = rule {
    zeroOrMore(nameValue, comma) ~ optional(comma)
  }

  def key: Rule1[String] = rule {
    keep(oneOrMore(("A" - "Z") | ("a" - "z") | ("0" - "9") | anyOf("-_:"))) ~ WS
  }

  def entry: Rule1[BibtexEntry] = rule {
    "@" ~ keep(name) ~ WS ~ "{" ~ WS ~ key ~ comma ~ nameValueList ~ "}" ~ WS ~~> BibtexEntry.apply
  }

  def endline: Rule0 = zeroOrMore(noneOf("\n\r")) ~ oneOrMore(anyOf("\n\r"))
  def comment: Rule0 = rule {
    // A comment is either a line that starts with @comment (case insensitive)
    ("@" ~ name ~? (_.toLowerCase == "comment") ~ endline
      // or a line that starts with something other than @
      | !"@" ~ endline)
  }

  def file: Rule1[List[BibtexEntry]] = rule {
    def subFile: Rule1[List[Option[BibtexEntry]]] =
      zeroOrMore(comment ~> (_ => None) | entry ~~> (Some(_)))
    subFile ~~> (_.flatten) ~ EOI
  }
}

object BibtexParser {
  /**
   * Reads a list of BibtexEntries.
   *
   * This does no error handling (besides printing the parse errors).
   */
  def parse(input: scala.io.Source): List[BibtexEntry] = {
    val parser = new BibtexParser
    val runner = RecoveringParseRunner(parser.file)
    val result = runner.run(input)
    result.parseErrors foreach println
    
    result.result.get
  }
  
  /**
   * Reads a single BibtexEntry.
   * 
   * This does no error handling.
   */
  def parseEntry(input: scala.io.Source): Option[BibtexEntry] = {
    val parser = new BibtexParser
    val runner = ReportingParseRunner(parser.entry)
    runner.run(input).result
  }
}

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
  def delimitedString(leftDelim: String, char: => Rule0, rightDelim: String): Rule0 = {
    leftDelim ~ zeroOrMore(char) ~ rightDelim
  }

  /**
   * This is a convenience overload for `delimitedString`,
   * which is particularly useful for strings containing nested substrings.
   */
  def delimitedString(leftDelim: String,
    forbiddenChars: String,
    subString: => Rule0,
    rightDelim: String): Rule0 = {
    delimitedString(leftDelim, (oneOrMore(noneOf(forbiddenChars)) | subString), rightDelim)
  }

  /**
   * A braced string may contain arbitrarily nested braced strings, but cannot contain `@`.
   */
  def bracedString: Rule0 = rule { delimitedString("{", "{}@", bracedString, "}") }

  /**
   * This is a nested braced string that may contain `@`. It does not appear by itself in bibtex,
   * but it appears as a substring of a quoted string.
   */
  def bracedAtString: Rule0 = rule { delimitedString("{", "{}", bracedAtString, "}") }

  /**
   * A quoted string may contain nested braced strings (with `@`) but may not contain
   * nested quoted strings.
   */
  def quotedString: Rule0 = rule { delimitedString("\"", "\"{}", bracedAtString, "\"") }

  /**
   * Matches a legal name for either a block or an entry.
   */
  def name: Rule0 = oneOrMore(("A" - "Z") | ("a" - "z") | "_")

  def number: Rule0 = oneOrMore("0" - "9")

  /**
   * Matches the right hand side of a "name = value" pair.
   */
  def value: Rule0 = rule { quotedString | bracedString | number }

  def WS: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  def comma: Rule0 = rule { "," ~ WS }

  /**
   * Matches a "name = value" pair.
   */
  def nameValue: Rule1[(String, String)] = rule {
    (keep(name) ~ WS ~ "=" ~ WS ~ keep(value) ~ WS) ~~> (_ -> _)
  }

  def nameValueList: Rule1[List[(String, String)]] = rule {
    zeroOrMore(nameValue, comma) ~ optional(comma)
  }

  def key: Rule1[String] = rule {
    keep(oneOrMore(("A" - "Z") | ("a" - "z") | ("0" - "9") | anyOf("-_:"))) ~ WS
  }

  def entry: Rule1[BibtexEntry] = rule {
    "@" ~ keep(name) ~ WS ~ "{" ~ WS ~ key ~ comma ~ nameValueList ~ "}" ~ WS ~~> BibtexEntry
  }

  def file: Rule1[List[BibtexEntry]] = rule {
    oneOrMore(entry)
  }
}

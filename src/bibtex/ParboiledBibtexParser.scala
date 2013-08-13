package bibtex

import org.parboiled.scala._

class ParboiledBibtexParser extends Parser {
  // keep("a") makes a rule that matches a and pushes it onto the value stack.
  def keep(r: Rule0): Rule1[String] = r ~> identity
  
  /** Creates a rule to match a delimited string.
   * 
   * This will match strings which start with `leftDelim`, then contain some
   * characters matching `char`, and terminate with `rightDelim`. 
   */
  def delimitedString(leftDelim: String, char: => Rule0, rightDelim: String): Rule0 = {
    rule {leftDelim ~ zeroOrMore(char) ~ rightDelim}
  }
  
  /** This is a convenience overload for `delimitedString`,
   * which is particularly useful for strings containing nested substrings.
   */
  def delimitedString(leftDelim: String,
                      forbiddenChars: String,
                      subString: => Rule0,
                      rightDelim: String): Rule0 = {
    delimitedString(leftDelim, (oneOrMore(noneOf(forbiddenChars)) | subString), rightDelim)
  }
    
  /** A braced string may contain arbitrarily nested braced strings, but cannot contain `@`.
   */
  def bracedString: Rule0 = delimitedString("{", "{}@", bracedString, "}")
  
  /** This is a nested braced string that may contain `@`. It does not appear by itself in bibtex,
   * but it appears as a substring of a quoted string.
   */
  def bracedAtString: Rule0 = delimitedString("{", "{}", bracedAtString, "}")
  
  /** A quoted string may contain nested braced strings (with `@`) but may not contain
   * nested quoted strings.
   */
  def quotedString: Rule0 = delimitedString("\"", "\"{}", bracedAtString, "\"")
  
  /** Matches a legal name for either a block or an entry.
   */
  def name: Rule0 = oneOrMore(("A" - "Z") | ("a" - "z") | "_")
  
  def number: Rule0 = oneOrMore("0" - "9")
  
  /** Matches the right hand side of a "name = value" pair.
   */
  def value: Rule0 = quotedString | bracedString | number
  
  def WS: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  
  /** Matches a "name = value" pair.
   */
  def nameValue: Rule2[String, String] = rule {
    keep(name) ~ WS ~ "=" ~ WS ~ keep(value) ~ WS
  }
  
  def nameValueList: Rule1[List[(String, String)]] = rule {
    zeroOrMore(nameValue, "," ~ WS) ~ optional(",") ~ WS
  }
  
  def key: Rule1[String] =
    keep(oneOrMore(("A" - "Z") | ("a" - "z") | ("0" - "9") | anyOf("-_:")))
  
  def entry: Rule1[BibtexEntry] = rule {
    "@" ~ keep(name) ~ WS ~ "{" ~ WS ~ key ~ WS ~ nameValueList ~ WS ~ "}" ~~> BibtexEntry
  }
}

case class PseudoChar(s: String, special: Boolean = false) {
  def braced: Boolean = s(0) == '{'
  def charCase: Case = {
    def oneCharCase(c: Char): Case = {
      // In bibtex, a digit is considered lower case.
      if (c.isLower || c.isDigit) {
        Lower()
      } else if (c.isUpper) {
        Upper()
      } else {
        Uncased()
      }
    }

    if (this.braced) {
      if (special) {
        s.find(_.isLetterOrDigit) match {
          case Some(c) => oneCharCase(c)
          case None => Uncased()
        }
      } else {
        Uncased()
      }
    } else oneCharCase(s(0))
  }
  
  def hasCase: Boolean = this.charCase != Uncased()
  
  override def toString: String = s
}

/**
 * A class for parsing bibtex values (i.e., things on the right hand side of an '-').
 * This does a little more work than the plain bibtex parser above, since it does
 * things like substitute latex combining characters.
 */
class EntryParser extends Parser {

  // keep("a") makes a rule that matches a and pushes it onto the value stack.
  def keep(r: Rule0): Rule1[String] = r ~> identity

  def texLetter = rule { "a" - "z" | "A" - "Z" }
  def digit = rule { "0" - "9" }
  
  // Do a TeX command substitution (e.g. "\o" -> "ø")
  private def texCharSubstitution(s: String): String = {
    TexChars.charSubstitutions.getOrElse(s, "\\" + s)
  }
  
  // Do a TeX accent substitution (e.g. ("^", "e") -> "ê"). We also allow the
  // character "e" to be inside braces, like "{e}". However, nested braces are not
  // allowed.
  private def texAccentSubstitution(acc: String, s: String): String = {
    def error(msg: String): String = {
      println(msg) // TODO: better error reporting
      "\\" + acc + s
    }
    
    println(s"Accent substitution $acc $s")
    if (s.length == 1 || (s.length == 3 && s(0) == '{' && s(2) == '}')) {
      val char = if (s.length == 1) s(0) else s(1)
      TexChars.charCompositions.get(acc + char) match {
        case Some(str) => str
        case None => error(s"Could not find a combining character for '$acc' and '$s'")
      }
    } else {
      error(s"Don't know how to put an accent on '$s'")
    }
  }
  
  def bracedString: Rule1[String] = rule {
    val char: Rule1[String] = rule { keep(noneOf("{}\\")) | texCommand | texAccent }
    (keep("{") ~ oneOrMore(char | bracedString) ~ keep("}")) ~~>
      ((a, b, c) => {a + b.mkString + c})
  }
  
 // A TeX command name can only contain letters, or it can consist of a single digit.
  // (This is not quite true because TeX has very flexible lexing rules, but it's
  // close enough.)
  def texCommand: Rule1[String] = rule {
    ("\\" ~ (oneOrMore(texLetter) | digit)) ~> texCharSubstitution
  }
  
  private val accentChars = "'`.~^\"="
  def texAccent: Rule1[String] = rule {
    ("\\" ~ keep(anyOf(accentChars)) ~ (keep(texLetter) | bracedString | texCommand)) ~~>
      texAccentSubstitution
  }
  
  // If a braced string which occurs at toplevel has a backslash as its first
  // non-brace character then it is "special." This gives it special treatment
  // in terms of determining its case.
  def toplevelBracedString: Rule1[PseudoChar] = rule {
    def isSpecial(s: String): Boolean = s(1) == '\\'
    (bracedString ~> isSpecial) ~~> (PseudoChar(_, _))
  }

  // A pseudo-character is either
  // - a single character which is not a brace, comma, or whitespace
  // - a balanced brace expression.
  def pseudoChar: Rule1[PseudoChar] = rule {
    val whitespaceChars = " \t\n\u000B\f\r"
    val normalChar: Rule1[String] = keep(noneOf(whitespaceChars + ",{}"))
    toplevelBracedString | normalChar ~~> (PseudoChar(_))
  }
}

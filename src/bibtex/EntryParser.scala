package bibtex

import org.parboiled.scala._

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
      ((a, b, c) => { a + b.mkString + c })
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
  // - a single character which is not a brace (or another forbidden char)
  // - a balanced brace expression.
  // TODO: maybe tex commands and escapes should be included also?
  def pseudoChar(forbiddenChars: String): Rule1[PseudoChar] = rule {
    val normalChar: Rule1[String] = keep(noneOf(forbiddenChars + "{}"))
    toplevelBracedString | normalChar ~~> (PseudoChar(_))
  }
  
  
  def entry: Rule1[List[PseudoChar]] = zeroOrMore(pseudoChar(""))
}

object EntryParser {
  def parse(input: io.Source): List[PseudoChar] = {
    val parser = new EntryParser
    val runner = ReportingParseRunner(parser.entry)
    runner.run(input).result.get
  }
}

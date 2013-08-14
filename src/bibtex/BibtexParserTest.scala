package bibtex

import org.scalatest.verb.BehaveWord
import org.parboiled.scala.testing.ParboiledTest
import org.parboiled.scala.ReportingParseRunner
import org.parboiled.scala.TracingParseRunner
import org.scalatest.FlatSpec
import org.scalatest.events.Formatter
import scala.collection.immutable.Map

class BibtexParserBasicTest extends ParboiledTest with FlatSpec {
  val p = new BibtexParser()
  type Result = String
  
  behavior of "BibtexParser's delimited string parsers"
  
  it should "parse simple braced strings" in {
    parse(ReportingParseRunner(p.keep(p.bracedString)), "{test}") {
      assert(parsingResult.result == Some("{test}"))
    }
  }
  
  it should "parse nested braced strings" in {
    parse(ReportingParseRunner(p.keep(p.bracedString)), "{{}a{a{}}}") {
      assert(parsingResult.result == Some("{{}a{a{}}}"))
    }
  }
  
  it should "fail on @ inside a braced string" in {
    failParse(ReportingParseRunner(p.keep(p.bracedString)), "{test@test}") {
      assert(parsingResult.parseErrors(0).getStartIndex() == 5)
    }
  }
  
  behavior of "BibtexParser's value parser"
  
  it should "parse braced strings" in {
    parse(ReportingParseRunner(p.keep(p.value)), "{Chan, Siu On and Mossel, Elchanan}") {
      assert(parsingResult.result === Some("{Chan, Siu On and Mossel, Elchanan}"))
    }
  }
}

class BibtexParserEntryTest extends ParboiledTest with FlatSpec {
  val p = new BibtexParser()
  type Result = BibtexEntry
  
  behavior of "BibtexParser's BibtexEntry parser"
  
  it should "parse an example entry" in {
    val entry =
"""@article{ChMoNe:12,
  author = {Chan, Siu On and Mossel, Elchanan and Neeman, Joe},
  title = {On extracting common random bits from correlated sources on large alphabets},
  archivePrefix = {arXiv},
  eprint = {1208.5946},
  note = {preprint},
  year = 2012,
}
"""
    parse(ReportingParseRunner(p.entry), entry) {
      assert(parsingResult.result.get.title ===
        "{On extracting common random bits from correlated sources on large alphabets}")
    }
  }
}

class EntryParserTest extends ParboiledTest with FlatSpec {
  val p = new EntryParser()
  type Result = String
  
  behavior of "EntryParser's text substitution"
  
  it should "replace TeX escapes by suitable characters" in {
    parse(ReportingParseRunner(p.texCommand), "\\l") {
      assert(parsingResult.result === Some("ł"))
    }
    parse(ReportingParseRunner(p.bracedString), "{b{\\l}ah}") {
      assert(parsingResult.result === Some("{b{ł}ah}"))
    }
  }
  it should "replace TeX combining sequences by suitable characters" in {
    parse(ReportingParseRunner(p.bracedString), "{bl\\'ah}") {
      assert(parsingResult.result === Some("{bláh}"))
    }
    parse(ReportingParseRunner(p.bracedString), "{bl{\\'a}h}") {
      assert(parsingResult.result === Some("{bl{á}h}"))
    }
    parse(ReportingParseRunner(p.bracedString), "{bl\\'{a}h}") {
      assert(parsingResult.result === Some("{bláh}"))
    }
  }
}
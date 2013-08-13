package bibtex

import org.scalatest.verb.BehaveWord
import org.parboiled.scala.testing.ParboiledTest
import org.parboiled.scala.ReportingParseRunner
import org.parboiled.scala.TracingParseRunner
import org.scalatest.FlatSpec
import org.scalatest.events.Formatter
import scala.collection.immutable.Map

class BibtexParserTest extends ParboiledTest with FlatSpec {
  val p = new ParboiledBibtexParser()
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
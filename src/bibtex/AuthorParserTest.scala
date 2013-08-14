package bibtex

import org.scalatest.verb.BehaveWord
import org.parboiled.scala.testing.ParboiledTest
import org.parboiled.scala.ReportingParseRunner
import org.parboiled.scala.Rule1
import org.scalatest.FlatSpec
import org.scalatest.events.Formatter
import scala.collection.immutable.Map

class AuthorParserBasicTest extends ParboiledTest with FlatSpec {
  val p = new AuthorParser()
  type Result = Word
  
  behavior of "AuthorParser's basic parsers"
  
  it should "parse braced strings as single characters" in {
    parse(ReportingParseRunner(p.word), "bl{df }ah other") {
      assert(!parsingResult.result.isEmpty)
      assert(parsingResult.result.get.toString == "bl{df }ah")
    }
  }
  
  it should "treat cased words correctly" in {
    parse(ReportingParseRunner(p.word), "blah") {
      assert(parsingResult.result.get.wordCase === Lower())
    }
    parse(ReportingParseRunner(p.word), "Blah") {
      assert(parsingResult.result.get.wordCase === Upper())
    }
    parse(ReportingParseRunner(p.word), "{Blah}") {
      assert(parsingResult.result.get.wordCase === Uncased())
    }
  }
}

trait TableTest extends ParboiledTest with FlatSpec {
   def checkTable(rule: Rule1[Result], tab: Map[String, Result]) {
    for ((input, name) <- tab) {
      parse(ReportingParseRunner(rule), input) {
        assert(parsingResult.result.get === name)
      }
    }
  }
}

class AuthorParserNameTest extends TableTest {
  val p = new AuthorParser()
  type Result = Name
  
 
  behavior of "AuthorParser's firstVonLast parser"
  
  val firstVonLast = Map(
      "jean de la fontaine" -> Name("", "jean de la", "fontaine", ""),
      "Jean de la fontaine" -> Name("Jean", "de la", "fontaine", ""),
      "Jean {de} la fontaine" -> Name("Jean de", "la", "fontaine", ""),
      "jean {de} {la} fontaine" -> Name("", "jean", "de la fontaine", ""),
      "Jean {de} {la} fontaine" -> Name("Jean de la", "", "fontaine", ""),
      "Jean De La Fontaine" -> Name("Jean De La", "", "Fontaine", ""),
      "jean De la Fontaine" -> Name("", "jean De la", "Fontaine", ""),
      "Jean de La Fontaine" -> Name("Jean", "de", "La Fontaine", "")
      )

  it should "correctly pass the \"Jean de la Fontaine\" test" in {
    checkTable(p.firstVonLast, firstVonLast)
  }
  
  behavior of "AuthorParser's vonLastFirst parser"
  
  val vonLastFirst = Map(
      "jean de la fontaine," -> Name("", "jean de la", "fontaine", ""),
      "de la fontaine, Jean" -> Name("Jean", "de la", "fontaine", ""),
      "De La Fontaine, Jean" -> Name("Jean", "", "De La Fontaine", ""),
      "De la Fontaine, Jean" -> Name("Jean", "De la", "Fontaine", ""),
      "de La Fontaine, Jean" -> Name("Jean", "de", "La Fontaine", "")
      )
      
  it should "correctly pass the \"Jean de la Fontaine\" test" in {
    checkTable(p.vonLastFirst, vonLastFirst)
  }
}

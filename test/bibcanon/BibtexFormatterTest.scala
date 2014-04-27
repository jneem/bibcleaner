package bibcanon

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import bibtex.Name
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll

@RunWith(classOf[JUnitRunner])
class BibtexFormatterTest extends FlatSpec with Checkers {
  behavior of "BibtexFormatter"

  it should "format some given author names as expected" in {
    val nameTable = Map(
      Name("Joe", "Neeman") -> "Neeman, Joe",
      Name("Ludwig", "van", "Beethoven", "jr") -> "van Beethoven, jr, Ludwig",
      Name("Ludwig", "van", "beethoven", "jr") -> "van {beethoven}, jr, Ludwig",
      Name("Mr.", "and") -> "{a{nd}}, Mr."
      )

    nameTable foreach {
      case (name, expected) =>
        assert(BibtexFormatter.author2Bibtex(name) === expected)
    }
  }
  
  it should "be a one-sided inverse of AuthorParser.parse" in {
    check(new BibtexFormatterSpecification)
  }
}

class BibtexFormatterSpecification extends Properties("BibtexFormatter") {
  import bibtex.AuthorParser
  
  lazy val genWord: Gen[String] = for {
    c <- Gen.alphaChar
    w <- Gen.oneOf(Gen.const(""), genWord)
  } yield c.toString + w
  
  lazy val genLowerWord = genWord filter (s => s(0).isLower)
  
  lazy val genOneNonEmptyName: Gen[String] = for {
    w <- genWord
    ws <- genOneName
  } yield (w + " " + ws).trim
  
  lazy val genOneName: Gen[String] = Gen.oneOf(Gen.const(""), genWord)
  
  // There is no way to get BibTex to recognize a "von" name except if its last part
  // is lower case.
  lazy val genVon: Gen[String] = {
    lazy val nonEmptyVon = for {
      ws <- genOneName
      w <- genLowerWord
    } yield (ws + " " + w).trim
    Gen.oneOf(Gen.const(""), nonEmptyVon)
  }
  
  val genName: Gen[Name] = for {
    first <- genOneName
    von <- genVon
    last <- genOneNonEmptyName
    jr <- genOneName
  } yield Name(first, von, last, jr)
  
  implicit val arbName: Arbitrary[Name] = Arbitrary(genName)
  
  property("one-sided inverse") = forAll { (n:Name) =>
    val source = io.Source.fromString(BibtexFormatter.author2Bibtex(n))
    val result = AuthorParser.parse(source)
    result.head == n
  }
}
package bibcanon

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec

@RunWith(classOf[JUnitRunner])
class AuxCitationScannerTest extends FlatSpec {
  behavior of "AuxCitationScanner"
  
  it should "match lines containing \\citation" in {
    val text = io.Source.fromString("foobar\n\\bibcite{fake}\n\\citation{real}\n")
    assert(AuxCitationScanner.scan(text).toList === List("real"))
  }
}

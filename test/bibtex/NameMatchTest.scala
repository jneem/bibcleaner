package bibtex

import org.parboiled.scala.testing.ParboiledTest
import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NameMatchTest extends FlatSpec {
  behavior of "Name's matches method"
  
  it should "match on empty names" in {
    assert(Name("", "Bartlett") matches Name("Peter", "Bartlett"))
  }
  
  it should "match on initials" in {
    assert(Name("P.", "Bartlett") matches Name("Peter", "Bartlett"))
  }
  
  it should "match on partial sequences of initials" in {
    assert(Name("P.L.", "Bartlett") matches Name("Peter", "Bartlett"))
    assert(Name("P L", "Bartlett") matches Name("Peter", "Bartlett"))
    assert(Name("P", "Bartlett") matches Name("Peter L", "Bartlett"))
  }
  
  it should "fail to match on different names" in {
    assert(!(Name("Paul", "Bartlett") matches Name("Peter", "Bartlett")))
    assert(!(Name("P", "Bartletti") matches Name("Peter", "Bartlett")))
  }
  
  it should "fail to match on incorrect initials" in {
    assert(!(Name("Q", "Bartlett") matches Name("Peter", "Bartlett")))
  }
}

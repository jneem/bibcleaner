package bibcanon.data

import bibcanon._
import java.text.Normalizer
import bibtex.BibtexEntry
import bibtex.Name
import scala.slick.driver.JdbcProfile
import scala.slick.jdbc.JdbcBackend.Database


trait Utils {
  class RedundancyException[T](redundantElements: Iterable[T]) extends Exception
  class UnknownTypeException(typ: String) extends Exception {
    override def toString = super.toString + ": " + typ
  }

  // When querying the tables, we normalize the text (by, e.g., removing accents).
  protected def normalize(s: String): String = {
    // Replace combined characters by normal characters + combining sequences.

    val s1 = Normalizer.normalize(s, Normalizer.Form.NFKD)
    // Remove the combining sequences.
    val s2 = s1.replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    // Replace multiple whitespaces with a single space.
    val s3 = s2.replaceAll("\\s+", " ")
    s3.trim.toLowerCase
  }
}


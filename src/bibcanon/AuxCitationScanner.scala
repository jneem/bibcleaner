package bibcanon

import java.util.regex._
import bibtex._

object AuxCitationScanner {
  /**
   * Scans a '.aux' file for a list of citation keys.
   */
  def scan(input: io.Source): Iterator[String] = {
    val citePat = Pattern.compile("\\\\citation\\{([^}]*)\\}")
    
    for (line <- input.getLines() if citePat.matcher(line).matches()) yield {
      val m = citePat.matcher(line)
      
      // We already know that the pattern matches, but we need to
      // call m.matches so that m.group will work.
      m.matches()
      m.group(1)
    }
  }
  
  def filter(entries: Iterator[BibtexEntry], auxInput: io.Source): Iterator[BibtexEntry] = {
    val keys = scan(auxInput).toSet
    entries filter (keys contains _.key)
  }

}
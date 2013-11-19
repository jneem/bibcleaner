package bibcanon

import bibtex._
import concurrent._
import concurrent.duration._

object Run {
  def main(args: Array[String]) {
    val BIB_FILE = "/home/jneeman/math/all-utf8.bib"
    val AUX_FILE = "/home/jneeman/math/isoperimetry/borell.aux"

    val entries = BibtexParser.parse(io.Source.fromFile(BIB_FILE))
    entries foreach (e => println(e.key))
    val filteredEntries = AuxCitationScanner.filter(entries.iterator, io.Source.fromFile(AUX_FILE))

    val db = new Database
    val pubs = filteredEntries map (e => (e, db canonicalizeBibtexEntry e))
    pubs.foreach {
      case (e, p) => {
        try {
          val pub = Await.result(p, Duration.Inf)
          println(pub.toBibtex(e.key))
        } catch {
          case ex: Exception => println(ex)
        }
      }
    }
  }
}
package bibcanon

import bibtex._
import concurrent._
import concurrent.duration._
import scala.slick.driver.H2Driver
import scala.slick.jdbc.JdbcBackend.Database

object Run {
  def main(args: Array[String]) {
    val BIB_FILE = "/home/jneeman/math/all-utf8.bib"
    val AUX_FILE = "/home/jneeman/math/isoperimetry/borell.aux"
    val DB_URL = "jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1"
    val DB_DRIVER ="org.h2.Driver"

    val entries = BibtexParser.parse(io.Source.fromFile(BIB_FILE))
    entries foreach (e => println(e.key))
    val filteredEntries = AuxCitationScanner.filter(entries.iterator, io.Source.fromFile(AUX_FILE))

    val connection = Database.forURL(DB_URL, DB_DRIVER)
    val db = new data.DataAccessLayer(H2Driver, connection)
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


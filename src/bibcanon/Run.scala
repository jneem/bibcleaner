package bibcanon

import bibtex._
import concurrent._
import concurrent.duration._

object Run {
  def main(args: Array[String]) {
    val FILE = "/home/jneeman/workspace/website/publications.bib"
    val result = BibtexParser.parse(io.Source.fromFile(FILE))
    val reg = result(0)
    println(reg.title)
    val db = new Database
    val pub: Publication = Await.result(db.canonicalizeBibtexEntry(reg), Duration.Inf)
    println(pub)
  }
}
package bibcanon

import java.net.URL
import collection.immutable.PagedSeq
import util.parsing.input.PagedSeqReader
import play.api.libs.json._
import bibtex._

object DOIQuerier {
  /**
   * Turns a JSON object (as returned from crossref.org) into a
   * BibtexEntry.
   */
  private def citeprocToBibtex(o: JsObject): BibtexEntry = {
    // TODO: only first and last names are currently handled. Check
    // what crossref returns for people with more complicated names.
    
    // TODO: better error checking for malformed JSON

    def authors(as: JsArray): List[Name] = {
      def author(a: JsObject): Name = Name((a \ "given").as[String], (a \ "family").as[String])
      as.value.toList map (x => author(x.asInstanceOf[JsObject]))
    }

    new PlainBibtexEntry("", "", Map())
  }

  /**
   * Query a DOI from dx.doi.org. This is a blocking method.
   */
  def query(doi: String): BibtexEntry = {
    val url = new URL(doi)
    println(doi)
    val connection = url.openConnection()
    connection.addRequestProperty("Accept", "application/citeproc+json")
    connection.connect()
    val inputStream = connection.getInputStream
    val output = io.Source.fromInputStream(connection.getInputStream).mkString

    Json.parse(output) match {
      case (o:JsObject) => citeprocToBibtex(o)
      case _ => throw new IllegalArgumentException("didn't get valid JSON")
    }
  }
}
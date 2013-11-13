package bibcanon

import java.net.{URL, URLEncoder}
import bibtex.BibtexEntry
import play.api.libs.json._

object CrossRefQuerier {
  /**
   * Given a query string, looks up the most relevant DOI using the
   * crossref.org metadata search.  This is a blocking method.
   */
  def query(q: String): String = {
    val query = URLEncoder.encode(q, "UTF-8") 
    val url = new URL(s"http://search.labs.crossref.org/dois?q=$query&sort=score")

    val connection = url.openConnection()
    connection.connect()

    // This gives us a JSON list of results, sorted by relevance.
    val output = io.Source.fromInputStream(connection.getInputStream).mkString("")
    Json.parse(output) match {
      // TODO: better exception handling
      case JsArray(a) => (a.head \ "doi").as[String]
      case _ => throw new IllegalArgumentException("got invalid json")
    } 
  }
  
  def query(e: BibtexEntry): String = query(e.title + (e.authors map (_.toString)).mkString(", "))
}
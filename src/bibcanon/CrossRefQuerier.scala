package bibcanon

import util.parsing.json
import java.net.{URL, URLEncoder}
import bibtex.BibtexEntry

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
    val outputJSON = json.JSON.parseFull(output) match {
      // TODO: throw a more meaningful exception
      case None => throw new IllegalArgumentException("didn't get valid JSON")
      case Some(o) => o.asInstanceOf[List[Any]]
    }
    if (outputJSON.isEmpty) {
      throw new IllegalArgumentException("didn't get any results")
    }

    // Extract the DOI from the most relevant result. 
    val bestMatch = outputJSON.head.asInstanceOf[Map[String,Any]]
    bestMatch("doi").asInstanceOf[String]
  }
  
  def query(e: BibtexEntry): String = query(e.title + (e.authors map (_.toString)).mkString(", "))
}
package bibcanon

import java.net.URL
import collection.immutable.PagedSeq
import util.parsing.input.PagedSeqReader
import bibtex.{BibtexEntry,BibtexParser}

object DOIQuerier {
  /**
   * Query a DOI from dx.doi.org. This is a blocking method.
   * 
   * TODO: rather than getting bibtex and parsing it, ask for
   * "application/citeproc+json" instead. Crossref doesn't seem to
   * escape its bibtex properly.
   */
  def query(doi: String): BibtexEntry = {
    val url = new URL(doi)
    println(doi)
    val connection = url.openConnection()
    connection.addRequestProperty("Accept", "application/x-bibtex")
    connection.connect()
    val inputStream = connection.getInputStream
    val output = io.Source.fromInputStream(connection.getInputStream)
    
    BibtexParser.parseEntry(output) match {
      case Some(b) => b
      case None => throw new IllegalArgumentException("Couldn't parse dx.doi's bibtex")
    }
  }
}
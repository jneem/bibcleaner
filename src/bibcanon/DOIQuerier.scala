package bibcanon

import java.net.URL
import collection.immutable.PagedSeq
import util.parsing.input.PagedSeqReader
import bibtex.{BibtexEntry,BibtexParser}

object DOIQuerier {
  /**
   * Query a DOI from dx.doi.org. This is a blocking method.
   */
  def query(doi: String): BibtexEntry = {
    val url = new URL(s"http://dx.doi.org/$doi")
    val connection = url.openConnection()
    connection.addRequestProperty("Accept", "application/x-bibtex")
    connection.connect()
    val output = io.Source.fromInputStream(connection.getInputStream)
    
    // Now parse the output using BibtexParser.
    val reader = new PagedSeqReader(PagedSeq.fromSource(output))
    val parser = new BibtexParser
    BibtexParser.parseEntry(output) match {
      case Some(b) => b
      case None => throw new IllegalArgumentException("Couldn't parse dx.doi's bibtex")
    }
  }
}
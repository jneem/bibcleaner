package bibcanon

import java.net.URL
import collection.immutable.PagedSeq
import util.parsing.input.PagedSeqReader
import play.api.libs.json._
import bibtex._

object DOIQuerier {
  class DOIJsonException extends RuntimeException
  
  /**
   * Turns a JSON object (as returned from crossref.org) into a
   * BibtexEntry.
   */
  private def parseCiteproc(o: JsObject): PublicationRecord = {
    // TODO: only first and last names are currently handled. Check
    // what crossref returns for people with more complicated names.
    
    def getAuthors(as: JsValue): List[Name] = {
      try {
        def author(a: JsObject): Name = Name((a \ "given").as[String], (a \ "family").as[String])
        as.asInstanceOf[JsArray].value.toList map (x => author(x.asInstanceOf[JsObject]))
      } //catch {
        // TODO: give a more informative message
        //case ex: Exception => throw new DOIJsonException
      //}
    }
    
    def getPages(p: JsValue): Option[Range] = {
      def parseRange(s: String): Range = {
        val ss = s split '-'
        try {
          ss(0).toInt to ss(1).toInt
        } catch {
          case ex: Exception => throw new DOIJsonException
        }
      }
      p.asOpt[String] map parseRange
    }

    new PublicationRecord {
      def title = (o \ "title").as[String]
      def authors = getAuthors(o \ "author")
      def year = (o \ "issued" \ "date-parts")(0)(0).asOpt[Int]
      def volume = (o \ "volume").asOpt[Int]
      def issue = (o \ "issue").asOpt[Int]
      def publisher = (o \ "publisher").asOpt[String]
      def doi = (o \ "DOI").asOpt[String]
      def containerTitle = (o \ "container-title").asOpt[String]
      def pages = getPages(o \ "page")
      def publicationType = (o \ "type").as[String]
    }
  }

  /**
   * Query a DOI from dx.doi.org. This is a blocking method.
   */
  def query(doi: String): PublicationRecord = {
    val url = new URL(doi)
    println(doi)
    val connection = url.openConnection()
    connection.addRequestProperty("Accept", "application/citeproc+json")
    connection.connect()
    val inputStream = connection.getInputStream
    val output = io.Source.fromInputStream(connection.getInputStream).mkString

    Json.parse(output) match {
      case (o:JsObject) => parseCiteproc(o)
      case _ => throw new DOIJsonException
    }
  }
}
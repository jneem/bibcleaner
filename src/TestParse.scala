import collection.immutable.PagedSeq
import util.parsing.input.PagedSeqReader
import bibtex.BibtexEntry

object TestParse {
  def main(args: Array[String]) {
    val parser = new BibtexParser
    val input = io.Source.fromFile(args(0))
    val reader = new PagedSeqReader(PagedSeq.fromSource(input))

    val bibtexEntries: parser.ParseResult[List[BibtexEntry]] = parser.file(reader)
    bibtexEntries match {
      case parser.Success(entries, _) => entries.foreach(e => println(e.title))
      case parser.NoSuccess(msg, _) => println(msg)
    }
  }
}
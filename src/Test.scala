import util.parsing.input.CharSequenceReader

object Test {
  def main(args: Array[String]) {
    val p = new BibtexParser
    val result: p.ParseResult[String] = p.bracedString(new CharSequenceReader("{t{sub}a{sub2}"))
    println(result)
  }
}
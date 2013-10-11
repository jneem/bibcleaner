package bibtex

case class Name(first: String, von: String, last: String, jr: String) {
  // TODO: handle middle initial: "Peter L" -> "P. L."
  private def initial(n: String): String =
    if (n.isEmpty) "" else (n(0) + ".")
  def full = List(first, von, last, jr).filter(_ != "").mkString(" ")
  def short = List(initial(first), von, last, jr).filter(_ != "").mkString(" ")
  def sanitized = Name(first.filter(_.isLetter).capitalize,
    von.filter(_.isLetter).capitalize,
    last.filter(_.isLetter).capitalize,
    jr.filter(_.isLetter).capitalize)
}


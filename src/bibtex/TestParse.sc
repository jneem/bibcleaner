package bibtex

import org.parboiled.scala._

object TestParse {
  val FILE = "/home/jneeman/math/elchanan.bib"    //> FILE  : String = /home/jneeman/math/elchanan.bib
  val parser = new BibtexParser                   //> parser  : bibtex.BibtexParser = bibtex.BibtexParser@267d5b1a
  val input = io.Source.fromFile(FILE)            //> input  : scala.io.BufferedSource = non-empty iterator
  val runner = ReportingParseRunner(parser.file)  //> runner  : org.parboiled.scala.parserunners.ReportingParseRunner[List[bibtex.
                                                  //| BibtexEntry]] = org.parboiled.scala.parserunners.ReportingParseRunner@39dc25
                                                  //| c9

  val result = runner.run(input)                  //> result  : org.parboiled.scala.ParsingResult[List[bibtex.BibtexEntry]] = org.
                                                  //| parboiled.scala.ParsingResult@3aa3cf94
  result.parseErrors map (_.getErrorMessage())    //> res0: List[String] = List()
  result.result.get map (_.title)                 //> res1: List[String] = List({Publications of Elchanan Mossel}, "")
  result.result.get                               //> res2: List[bibtex.BibtexEntry] = List(Map(type -> {Personal bibliography}, s
                                                  //| ource -> {based on data drawn from <a href = "http://www.ams.org/mathscinet/
                                                  //| search/publications.html?extend=1&fmt=bibtex&pg1=IID&s1=35285&r=1">MathSciNe
                                                  //| t</a> and other sources. Last updated Sun Nov 10 2007.}, title -> {Publicati
                                                  //| ons of Elchanan Mossel}, index -> {META}, author -> {Elchanan Mossel}, autho
                                                  //| r_url -> {http://www.stat.berkeley.edu/~mossel/}, heading -> {Publications o
                                                  //| f <a href ="http://www.stat.berkeley.edu/~mossel/">Elchanan Mossel</a>}), Ma
                                                  //| p(bool_head -> {Functions on Products Spaces and Influences}, comp_head -> {
                                                  //| Computational Complexity}, prob_head -> {Probability on Graphs}, index -> {S
                                                  //| UBJECTS}, alg_head -> {Algorithms}, misc_head -> {Misc}, bio_head -> {Bioinf
                                                  //| ormatics}, game_head -> {Game Theory, Social Choice and Economics}))
  
}
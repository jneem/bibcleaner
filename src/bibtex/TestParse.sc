package bibtex

import org.parboiled.scala._

object TestParse {
	def main() {
	}                                         //> main: ()Unit
  
  val FILE = "/home/jneeman/workspace/website/publications.bib"
                                                  //> FILE  : String = /home/jneeman/workspace/website/publications.bib
  val parser = new BibtexParser                   //> parser  : bibtex.BibtexParser = bibtex.BibtexParser@78100c56
  val input = io.Source.fromFile(FILE)            //> input  : scala.io.BufferedSource = non-empty iterator
  val runner = ReportingParseRunner(parser.file)  //> runner  : org.parboiled.scala.parserunners.ReportingParseRunner[List[bibtex.
                                                  //| BibtexEntry]] = org.parboiled.scala.parserunners.ReportingParseRunner@703c1a
                                                  //| 5

  val result = runner.run(input)                  //> result  : org.parboiled.scala.ParsingResult[List[bibtex.BibtexEntry]] = org.
                                                  //| parboiled.scala.ParsingResult@199155be
  result.parseErrors map (_.getErrorMessage())    //> res0: List[String] = List()
  result.result.get map (_.title)                 //> res1: List[String] = List(Regularization in kernel learning, ���1-regularize
                                                  //| d linear regression: persistence and oracle inequalities, Stochastic block m
                                                  //| odels and reconstruction, Robust Dimension Free Isoperimetry in {G}aussian S
                                                  //| pace, Robust optimality of {G}aussian noise stability, Majority is Stablest:
                                                  //|  Discrete and {SoS}, A law of large numbers for weighted plurality, Majority
                                                  //|  Dynamics and Aggregation of Information in Social Networks, On extracting c
                                                  //| ommon random bits from correlated sources on large alphabets, Belief Propaga
                                                  //| tion, Robust Reconstruction, and Optimal Recovery of Block Models, A multidi
                                                  //| mensional version of noise stability, A connection between surface area and 
                                                  //| noise sensitivity, Spectral redemption: clustering sparse networks)
  result.matched                                  //> res2: Boolean = true
  
  result.result.get.head("author")                //> res3: String = Mendelson, Shahar and Neeman, Joe
  result.result.get map (_.authors)               //> res4: List[List[bibtex.Name]] = List(List(Name(Shahar,,Mendelson,), Name(Joe
                                                  //| ,,Neeman,)), List(Name(Peter,,Bartlett,), Name(Shahar,,Mendelson,), Name(Joe
                                                  //| ,,Neeman,)), List(Name(Elchanan,,Mossel,), Name(Joe,,Neeman,), Name(Allan,,S
                                                  //| ly,)), List(Name(Elchanan,,Mossel,), Name(Joe,,Neeman,)), List(Name(Elchanan
                                                  //| ,,Mossel,), Name(Joe,,Neeman,)), List(Name(A.,,De,), Name(E.,,Mossel,), Name
                                                  //| (J.,,Neeman,)), List(Name(Joe,,Neeman,)), List(Name(Elchanan,,Mossel,), Name
                                                  //| (Joe,,Neeman,), Name(Omer,,Tamuz,)), List(Name(Siu On,,Chan,), Name(Elchanan
                                                  //| ,,Mossel,), Name(Joe,,Neeman,)), List(Name(Elchanan,,Mossel,), Name(Joe,,Nee
                                                  //| man,), Name(Allan,,Sly,)), List(Name(Joe,,Neeman,)), List(Name(Joe,,Neeman,)
                                                  //| ), List(Name(Florent,,Krzakala,), Name(Cristopher,,Moore,), Name(Elchanan,,M
                                                  //| ossel,), Name(Joe,,Neeman,), Name(Allan,,Sly,), Name(Lenka,,Zdeborov��,), Na
                                                  //| me(Pan,,Zhang,)))

}
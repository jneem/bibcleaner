package bibtex

import org.parboiled.scala._

object TestParse {
	def main() {
	}                                         //> main: ()Unit
  val FILE = "/home/jneeman/math/all.utf-8.bib"   //> FILE  : String = /home/jneeman/math/all.utf-8.bib
  val parser = new BibtexParser                   //> parser  : bibtex.BibtexParser = bibtex.BibtexParser@1fc2ab6c
  val input = io.Source.fromFile(FILE)            //> input  : scala.io.BufferedSource = non-empty iterator
  val runner = ReportingParseRunner(parser.file)  //> runner  : org.parboiled.scala.parserunners.ReportingParseRunner[List[bibtex.
                                                  //| BibtexEntry]] = org.parboiled.scala.parserunners.ReportingParseRunner@74a5ea
                                                  //| c1

  val result = runner.run(input)                  //> result  : org.parboiled.scala.ParsingResult[List[bibtex.BibtexEntry]] = org.
                                                  //| parboiled.scala.ParsingResult@7a431fd
  result.parseErrors map (_.getErrorMessage())    //> res0: List[String] = List()
  result.result.get map (_.title)                 //> res1: List[String] = List({Learning Factor Graphs in Polynomial Time and Sam
                                                  //| pling Complexity}, {Handbook of Mathematical Functions}, {Bayesian Learning 
                                                  //| in Social Networks}, {Spreading of sets in product spaces and hypercontracti
                                                  //| on of the Markov operator}, {Aggregating inconsistent information: ranking a
                                                  //| nd clustering}, {Monotone versus positive}, {Random walks on finite groups a
                                                  //| nd rapidly mixing {M}arkov
                                                  //|               chains}, {The $\zeta(2)$ Limit in Random Assignment Problem}, 
                                                  //| {Reversible {M}arkov chains and random walks on graphs}, {The identifiabilit
                                                  //| y of tree topology for phylogenetic models, including covarion and mixture m
                                                  //| odels}, {Graph products, {Fourier} analysis and spectral techniques}, {Ranki
                                                  //| ng Tournaments}, {The Probabilistic Method}, {The probabilistic method}, {Ti
                                                  //| ght bounds on Learnability of evolution}, {Integrated Coverage and Connectiv
                                                  //| ity in Wireless Sensor
                                                  //|                Networks: A Two-Dimensional Perc
                                                  //| Output exceeds cutoff limit.
  result.matched                                  //> res2: Boolean = true
  
}
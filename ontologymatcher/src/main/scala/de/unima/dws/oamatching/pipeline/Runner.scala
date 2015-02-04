package de.unima.dws.oamatching.pipeline

import java.io.File

import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.{OntologyLoader, AlignmentParser}
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.evaluation.EvaluationMatchingRunner

/**
 * Created by mueller on 28/01/15.
 */
object Runner extends App {
  MatcherRegistry.init
  //EvaluationMatchingRunner.matchAndEvaluateConference("ontos/2014/conference",Map(("threshold",0.6)))
  runRound()
  //runSingleStructural()

  def runSingleStructural():Unit = {
    val file_onto1: File = new File("ontos/2014/conference/cmt.owl")
    val file_onto2: File = new File("ontos/2014/conference/Conference.owl")
    val reference = AlignmentParser.parseRDF("ontos/2014/conference/reference-alignment/cmt-conference.rdf")

    val onto1  =OntologyLoader.load(file_onto1)
    val onto2 = OntologyLoader.load(file_onto2)
    val test_problem = MatchingProblem(onto1 ,onto2,"test")


    val base_element_matcher =  MatcherRegistry.getMatcherByName("simple_tfidf").get
    val initial_Alignment = base_element_matcher.align(test_problem,0.3)

    val base_struct_matcher = MatcherRegistry.structural_matcher_by_name.get("graphBasedUsedClassMatcher").get
    val enhanced_Alignment =  base_struct_matcher.align(test_problem,initial_Alignment,0.0)



   println(initial_Alignment.evaluate(reference))
    println(enhanced_Alignment.evaluate(reference))

   // println(reference.correspondences.size)
    //println(test._1.evaluate(reference))
    //println(test._1.toString)
  }



  def runRound():Unit = {

    EvaluationMatchingRunner.matchAndEvaluateConference(Config.PATH_TO_CONFERENCE, Map(("threshold",0.3)) )

  }
}

package de.unima.dws.oamatching.pipeline

import java.io.File

import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationMatchingTask, EvaluationMatchingTaskWithParameters, EvaluationMatchingRunner}
import de.unima.dws.oamatching.pipeline.util.MetaDataMgmt

/**
 * Created by mueller on 28/01/15.
 */
object Runner {
  MatcherRegistry.initLargeScale()

  //val problems = EvaluationMatchingRunner.parseConference("ontos/2014/conference");
  //runRound(0.01)

  //runSingleBaseMatcherForMultipleProblems("jaroMeasure",problems.toVector,0.8,"conference")

  //runSingleStructural("simFloodMatcher","word2Vec","ontos/2014/conference/cmt.owl","ontos/2014/conference/Conference.owl","ontos/2014/conference/reference-alignment/cmt-conference.rdf",0.0,"conference")

  /**
   * Runs a single structural Matcher based on a base matcher result
   * @param struct_matcher_name
   * @param base_matcher_name
   * @param onto1
   * @param onto2
   * @param ref
   * @param threshold
   * @param problem_name
   */
  def runSingleStructural(struct_matcher_name:String,base_matcher_name:String, onto1:String, onto2:String, ref:String, threshold:Double, problem_name:String):Unit = {

    val (reference: Alignment, test_problem: MatchingProblem) = prepareDataSet(onto1, onto2, ref,problem_name)

    val base_element_matcher =  MatcherRegistry.getMatcherByName(base_matcher_name).get

    //base matcher threshold
    val base_threshold = 0.0

    val initial_Alignment = base_element_matcher.align(test_problem,base_threshold)

    val base_struct_matcher = MatcherRegistry.structural_matcher_by_name.get(struct_matcher_name).get
    val enhanced_Alignment =  base_struct_matcher.align(test_problem,initial_Alignment,threshold)

    println(initial_Alignment.evaluate(reference))
    println(enhanced_Alignment.evaluate(reference))

  }


  /**
   * Runs the whole platform on conference
   * @param threshold
   */
  def runRound(threshold:Double):Unit = {
    //TODO include other datasets
    EvaluationMatchingRunner.matchAndEvaluateConference(Config.PATH_TO_CONFERENCE, Map(("threshold",threshold)) )

  }

  /**
   * Runs the platform on one particular matching problem
   * @param onto1
   * @param onto2
   * @param ref
   * @param threshold
   * @param problem_name
   */
  def runSinglePlatform(onto1:String, onto2:String, ref:String, threshold:Double, problem_name:String):Unit =  {
    val (reference: Alignment, test_problem: MatchingProblem) = prepareDataSet(onto1, onto2, ref,problem_name)

    val params = Map(("threshold",threshold))
    val task = EvaluationMatchingTaskWithParameters(test_problem, params, reference)
    val result = EvaluationMatchingRunner.matchAndEvaluateSingle(task)
    println("pipeline res")
    println(result.evaluationResult)
    println("best base")
    println(result.bestBaseMatcher)
    println("majority vote")
    println(result.majorityVoteResult)

  }


  /**
   * Runs a single base matcher for a given problem and parameters
   * @param matcher_name
   * @param onto1
   * @param onto2
   * @param ref
   * @param threshold
   */
  def runSingleBaseMatcher(matcher_name:String, onto1:String, onto2:String, ref:String, threshold:Double, problem_name:String):Unit = {
    val (reference: Alignment, test_problem: MatchingProblem) = prepareDataSet(onto1, onto2, ref,problem_name)
    val o_matcher =  MatcherRegistry.matcher_by_name(matcher_name)
    val o_threshold = MetaDataMgmt.getThreshold(test_problem.name, matcher_name).getOrElse(threshold)

    //check if threshold available
    val produced_alignment =  o_matcher.align(test_problem,o_threshold)
    val result = produced_alignment.evaluate(reference)
    println(result)
  }

  /**
   * Runs a base matcher for all problems and aggregates the results
   * @param matcher_name
   * @param problems
   * @param threshold fallback threshold
   * @param problem_name
   */
  def runSingleBaseMatcherForMultipleProblems(matcher_name:String,problems:Vector[EvaluationMatchingTask], threshold:Double, problem_name:String):AggregatedEvaluationResult = {
    val matcher =  MatcherRegistry.matcher_by_name(matcher_name)

    val base_threshold = MetaDataMgmt.getThreshold(problem_name, matcher_name).getOrElse(threshold)

    val results = problems.map(task => {
      val alignment = matcher.align(task.matching_problem,base_threshold)
      val res = alignment.evaluate(task.reference)

      res
    });

    val agg_res = EvaluationResultAggregator.aggregateEvaluationResults(results.toList)
    println(agg_res)
    agg_res
  }


  /**
   * Returns a matching problem for a
   * @param onto1
   * @param onto2
   * @param ref
   * @param data_set_name
   * @return
   */
  def prepareDataSet(onto1: String, onto2: String, ref: String, data_set_name:String): (Alignment, MatchingProblem) = {
    val file_onto1: File = new File(onto1)
    val file_onto2: File = new File(onto2)
    val reference = AlignmentParser.parseRDF(ref)

    val l_onto1 = OntologyLoader.load(file_onto1)
    val l_onto2 = OntologyLoader.load(file_onto2)
    val test_problem = MatchingProblem(l_onto1, l_onto2,data_set_name)
    (reference, test_problem)
  }


  def runEvaluateFromRapidminerFile(path:String,ref_file:String, threshold:Double): Unit = {
    val file:File = new File(path)
    val matchings = RapidminerJobs.readCSV(file)

    val selected =  MatchingSelector.greedyRankSelector(matchings,threshold)
    selected.foreach(matching => println(matching._1))
    val alignment = new Alignment(null,null, selected)


    alignment.correspondences.foreach(cell => println(cell))
    val reference = AlignmentParser.parseRDF(ref_file)

    println(alignment.evaluate(reference))

  }


}

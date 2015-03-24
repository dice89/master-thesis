package de.unima.dws.oamatching.pipeline

import java.io.File

import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationMatchingTask, EvaluationMatchingTaskWithParameters, EvaluationMatchingRunner}
import de.unima.dws.oamatching.pipeline.util.MetaDataMgmt

import scala.io.Source

/**
 * Created by mueller on 28/01/15.
 */
case class RunConfiguration(threshold:Double,normalization:String,data_set_name:String,path_to_dataset:String, matching_pipline: (MatchingProblem, Double, Double) => (Alignment, FeatureVector))
object Runner {
  //init stuff
  MatcherRegistry.initLargeScale()
  RapidminerJobs.init()
  //TODO spark init

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
   * Runs the whole platform on a specified dataset
   * @param config
   */
  def runRound(config:RunConfiguration):Unit = {
    EvaluationMatchingRunner.matchAndEvaluateConference(Config.PATH_TO_CONFERENCE, config )
  }

  /**
   *  Runs the platform on one particular matching problem
   * @param onto1
   * @param onto2
   * @param ref
   * @param runConfiguration
   */
  def runSinglePlatform(onto1:String, onto2:String, ref:String, runConfiguration: RunConfiguration):Unit =  {
    val (reference: Alignment, test_problem: MatchingProblem) = prepareDataSet(onto1, onto2, ref,runConfiguration.data_set_name)

    val task = EvaluationMatchingTaskWithParameters(test_problem, runConfiguration, reference)
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
    val matchings_and_dim = RapidminerJobs.readCSV(file)
    val matchings = matchings_and_dim._3
    val selected =  MatchingSelector.greedyRankSelectorSimple(matchings,threshold)
    selected.foreach(matching => println(matching._1))
    val alignment = new Alignment(null,null, selected)


    alignment.correspondences.foreach(cell => println(cell))
    val reference = AlignmentParser.parseRDF(ref_file)

    println(alignment.evaluate(reference))

  }


  def parseRunConfig():RunConfiguration = {

    val paramMap: Map[String, String] = Source.fromFile("config/pipeline_run_config.txt").getLines().map(line =>{
      val tuple = line.split("=")
      val key:String = tuple(0)
      val value:String = tuple(1)

      if(key.equals("thresh")){
        key->value
      }else if(key.equals("norm")){
        key->value
      }else if(key.equals("dsname")){
        key->value
      }else if(key.equals("pathtods")) {
        key -> value
      } else {
        ""->""
      }

    }).toMap



    //build pipeline
    val outlier_function= RapidminerJobs.rapidminerOutlierDetection(Config.OA_PROCESS, Config.OA_BASE_DIR)_
    val norm_function = ScoreNormalizationFunctions.getNormFunction(paramMap.get("norm").get)
    val matching_pipline: (MatchingProblem, Double, Double) => (Alignment, FeatureVector) =  MatchingPipelineCore.createMatchingPipeline(outlier_function)(norm_function)

    RunConfiguration(paramMap.get("thresh").get.toDouble, paramMap.get("norm").get,paramMap.get("dsname").get, paramMap.get("pathtods").get, matching_pipline)
  }


}

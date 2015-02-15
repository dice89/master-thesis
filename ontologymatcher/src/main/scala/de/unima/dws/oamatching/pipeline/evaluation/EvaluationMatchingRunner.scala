package de.unima.dws.oamatching.pipeline.evaluation

import java.io.File
import java.net.URI

import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.pipeline.util.ResultLogger
import de.unima.dws.oamatching.pipeline._

import scala.collection.JavaConversions.JEnumerationWrapper

/**
 * Created by mueller on 28/01/15.
 */

case class EvaluationMatchingTaskWithParameters(matching_problem: MatchingProblem, config: RunConfiguration, reference: Alignment)

case class EvaluationMatchingTask(matching_problem: MatchingProblem, reference: Alignment)


object EvaluationMatchingRunner {

  //TODO add benchmark
  def matchAndEvaluateConference(path_to_conf:String,config:RunConfiguration):Unit = {
    matchAndEvaluate(parseConference(path_to_conf),config,"conference")
  }

  def matchAndEvaluate(matchingTasks:Seq[EvaluationMatchingTask], config: RunConfiguration, run_name:String) = {
      val matching_tasks_with_parameters = matchingTasks.map(task => EvaluationMatchingTaskWithParameters(task.matching_problem,config, task.reference))

     val matching_task_results =matching_tasks_with_parameters.map(task => {
        matchAndEvaluateSingle(task)
      })

    //may not efficient way but prepare each single datastructure
    val base_matcher_results: Seq[Map[String, EvaluationResult]] = matching_task_results.map(result => result.baseMatcherResults)

    val best_base_matcher_results = matching_task_results.map(result => result.bestBaseMatcher)
    val majority_vote_results =  matching_task_results.map(result => result.majorityVoteResult)
    val core_pipeline_results =  matching_task_results.map(result => result.evaluationResult)


    //compute average result of all base matcher
    val average_base_matcher_results: Map[String, AggregatedEvaluationResult] = computeAggregatedResultsForAllBaseMatchers(base_matcher_results.toList)

    //get best average matcher by micro f1 measure -> baseline 1
    val best_average_base_matcher: (String, AggregatedEvaluationResult) =  average_base_matcher_results.maxBy{case(matcher,eval_res)=>(eval_res.micro_eval_res.f1Measure)}
    ResultLogger.log_result(run_name,"best_average_matcher_baseline-"+best_average_base_matcher._1,best_average_base_matcher._2);

    //compute average result of best base matcher -> baseline 2
    val average_best_base_matcher_result = computeAggregatedResults(best_base_matcher_results.unzip._2.toList)
    ResultLogger.log_result(run_name,"best_base_matcher_baseline",average_best_base_matcher_result);

    //compute average result of majority vote -> baseline 3
    val average_majority_vote_result = computeAggregatedResults(majority_vote_results.toList)
    ResultLogger.log_result(run_name,"majority_vote_baseline",average_majority_vote_result);

    //compute average result of platform
    val average_core_result = computeAggregatedResults(core_pipeline_results.toList)
    ResultLogger.log_result(run_name,"average_core_pipleine",average_core_result);


    var bet_base_lines:Int = 0;
    //compare to
    println("Baseline 1")
    println(best_average_base_matcher)
    if(average_core_result.micro_eval_res.f1Measure > best_average_base_matcher._2.micro_eval_res.f1Measure){
      println("bet")
      bet_base_lines = bet_base_lines+1
    }else {
      println("not bet")
    }
    println("Baseline 2")
    println(average_best_base_matcher_result)
    if(average_core_result.micro_eval_res.f1Measure > average_best_base_matcher_result.micro_eval_res.f1Measure){
      println("bet")
      bet_base_lines = bet_base_lines+1
    }else {
      println("not bet")
    }
    println("Baseline 3")
    println(average_majority_vote_result)
    if(average_core_result.micro_eval_res.f1Measure > average_majority_vote_result.micro_eval_res.f1Measure){
      println("bet")
      bet_base_lines = +bet_base_lines+1
    }else {
      println("not bet")
    }
    println("Computed Result")
    println(average_core_result);
    println("Micro F1Measure "+ average_core_result.micro_eval_res.f1Measure);
    println("Bet Baselines: "+ bet_base_lines)

  }



  /**
   * TODO Maybe make it composable
   * @param eval_task
   * @return
   */
  def matchAndEvaluateSingle(eval_task: EvaluationMatchingTaskWithParameters): EvaluationRoundResult = {

    Evaluation.evaluate(eval_task.matching_problem, eval_task.reference, eval_task.config)
  }


  def computeAggregatedResults(eval_results: List[EvaluationResult]): AggregatedEvaluationResult = {
    EvaluationResultAggregator.aggregateEvaluationResults(eval_results)
  }

  /**
   *
   * @param base_matcher_results
   * @return
   */
  def computeAggregatedResultsForAllBaseMatchers(base_matcher_results: List[Map[String, EvaluationResult]]): Map[String, AggregatedEvaluationResult] = {

    //get eval results by matcher
    val unique_elements: List[String] = base_matcher_results.map(eval_res_map => eval_res_map.keys).flatten.distinct
    val result_per_matcher: List[(String, List[Option[EvaluationResult]])] = unique_elements.map(matcher => (matcher, base_matcher_results.map { round_result => round_result.get(matcher)}))

    //now aggregate for each matcher
    val aggregated_result_per_matcher: Map[String, AggregatedEvaluationResult] = result_per_matcher.map { case (matcher_name, eval_results) => (matcher_name, EvaluationResultAggregator.aggregateEvaluationResultsOption(eval_results))}.toMap

    aggregated_result_per_matcher
  }

  /**
   * Parses the conference dataset from OAEI challenge
   * @param path_to_folder
   * @return
   */
  def parseConference(path_to_folder: String): Seq[EvaluationMatchingTask] = {
    val folder: File = new File(path_to_folder + File.separator + "reference-alignment/")

    val problems = for (ref_align_file <- folder.listFiles(new RdfFileFilter)) yield {

      val ontos: List[String] = ref_align_file.getName().split("-").toList
      val name_onto1: String = path_to_folder + File.separator + ontos(0).replaceAll("-", "") + ".owl"
      val name_onto2: String = path_to_folder + File.separator + ontos(1).replaceAll("-", "").replaceAll(".rdf", "") + ".owl"


      val onto1 = OntologyLoader.load(name_onto1)
      val onto2 = OntologyLoader.load(name_onto2)
      //parse alignments
      val reference: Alignment =  AlignmentParser.parseRDF(ref_align_file.getAbsolutePath())

      val name: String = ref_align_file.getName().dropRight(4)

      val matching_problem = MatchingProblem(onto1, onto2, name)

      EvaluationMatchingTask(matching_problem,reference)
    }

    problems
  }


  def parseSingle(f_onto1:String,f_onto2:String,f_reference:String):EvaluationMatchingTask = {

    //name of the problem is the name of the rdf file alignment without .rdf extension
    val name: String = f_reference.dropRight(4)
    val onto1 = OntologyLoader.load(f_onto1)
    val onto2 = OntologyLoader.load(f_onto2)
    val reference: Alignment =  AlignmentParser.parseRDF(f_reference)

    val matching_problem = MatchingProblem(onto1, onto2, name)

    EvaluationMatchingTask(matching_problem,reference)
  }

}

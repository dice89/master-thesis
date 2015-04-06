package de.unima.dws.oamatching.pipeline.evaluation

import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.pipeline._
import de.unima.dws.oamatching.pipeline.util.ResultLogger


/**
 * Created by mueller on 28/01/15.
 */

case class EvaluationMatchingTaskWithParameters(matching_problem: MatchingProblem, config: RunConfiguration, reference: Alignment)

case class EvaluationMatchingTask(matching_problem: MatchingProblem, reference: Alignment)


object EvaluationMatchingRunner extends EvaluationDataSetParser {


  def matchAndEvaluateConference(path_to_prob: String,problem:String, config: RunConfiguration): Unit = {
    if(Config.loaded_config.getBoolean("pipeline.compute_baselines")){
      matchAndEvaluate(parseProblems(problem,path_to_prob), config,problem)
    }else {
      matchAndEvaluateOnlyCore(parseProblems(problem,path_to_prob), config,problem)
    }

  }

  def matchAndEvaluateOnlyCore(matchingTasks: Seq[EvaluationMatchingTask], config: RunConfiguration, run_name: String) = {
    val matching_tasks_with_parameters = matchingTasks.map(task => EvaluationMatchingTaskWithParameters(task.matching_problem, config, task.reference))

    val matching_task_results: List[EvaluationResult] = matching_tasks_with_parameters.map(task => {
      matchAndEvaluateCorePlatformOnly(task)
    }).toList

    val average_core_result = computeAggregatedResults(matching_task_results)
    ResultLogger.log_result(run_name, "average_core_pipleine", average_core_result);
    println(average_core_result)
  }

  def matchAndEvaluate(matchingTasks: Seq[EvaluationMatchingTask], config: RunConfiguration, run_name: String) = {
    val matching_tasks_with_parameters = matchingTasks.map(task => EvaluationMatchingTaskWithParameters(task.matching_problem, config, task.reference))

    val matching_task_results = matching_tasks_with_parameters.map(task => {
      matchAndEvaluateSingle(task)
    })

    //may not efficient way but prepare each single datastructure
    val base_matcher_results: Seq[Map[String, EvaluationResult]] = matching_task_results.map(result => result.baseMatcherResults)

    val best_base_matcher_results = matching_task_results.map(result => result.bestBaseMatcher)
    val majority_vote_results = matching_task_results.map(result => result.majorityVoteResult)
    val core_pipeline_results = matching_task_results.map(result => result.evaluationResult)


    //compute average result of all base matcher
    val average_base_matcher_results: Map[String, AggregatedEvaluationResult] = computeAggregatedResultsForAllBaseMatchers(base_matcher_results.toList)

    //get best average matcher by micro f1 measure -> baseline 1
    val best_average_base_matcher: (String, AggregatedEvaluationResult) = average_base_matcher_results.maxBy { case (matcher, eval_res) => (eval_res.micro_eval_res.f1Measure)}
    ResultLogger.log_result(run_name, "best_average_matcher_baseline-" + best_average_base_matcher._1, best_average_base_matcher._2);

    //compute average result of best base matcher -> baseline 2
    val average_best_base_matcher_result = computeAggregatedResults(best_base_matcher_results.unzip._2.toList)
    ResultLogger.log_result(run_name, "best_base_matcher_baseline", average_best_base_matcher_result);

    //compute average result of majority vote -> baseline 3
    val average_majority_vote_result = computeAggregatedResults(majority_vote_results.toList)
    ResultLogger.log_result(run_name, "majority_vote_baseline", average_majority_vote_result);

    //compute average result of platform
    val average_core_result = computeAggregatedResults(core_pipeline_results.toList)
    ResultLogger.log_result(run_name, "average_core_pipleine", average_core_result);


    var bet_base_lines: Int = 0;
    //compare to
    println("Baseline 1")
    println(best_average_base_matcher)
    if (average_core_result.micro_eval_res.f1Measure > best_average_base_matcher._2.micro_eval_res.f1Measure) {
      println("bet")
      bet_base_lines = bet_base_lines + 1
    } else {
      println("not bet")
    }
    println("Baseline 2")
    println(average_best_base_matcher_result)
    if (average_core_result.micro_eval_res.f1Measure > average_best_base_matcher_result.micro_eval_res.f1Measure) {
      println("bet")
      bet_base_lines = bet_base_lines + 1
    } else {
      println("not bet")
    }
    println("Baseline 3")
    println(average_majority_vote_result)
    if (average_core_result.micro_eval_res.f1Measure > average_majority_vote_result.micro_eval_res.f1Measure) {
      println("bet")
      bet_base_lines = +bet_base_lines + 1
    } else {
      println("not bet")
    }
    println("Computed Result")
    println(average_core_result);
    println("Micro F1Measure " + average_core_result.micro_eval_res.f1Measure);
    println("Bet Baselines: " + bet_base_lines)

  }


  /**
   * TODO Maybe make it composable
   * @param eval_task
   * @return
   */
  def matchAndEvaluateSingle(eval_task: EvaluationMatchingTaskWithParameters): EvaluationRoundResult = {
    Evaluation.evaluate(eval_task.matching_problem, eval_task.reference, eval_task.config)
  }

  def matchAndEvaluateCorePlatformOnly(eval_task: EvaluationMatchingTaskWithParameters): EvaluationResult = {
    Evaluation.evaluateOnlyCore(eval_task.matching_problem, eval_task.reference, eval_task.config)
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
    val unique_elements: List[String] = base_matcher_results.map(eval_res_map => eval_res_map.keys).flatten.distinct.filterNot(_.contains("structural"))


    println(unique_elements)

    val result_per_matcher: List[(String, List[Option[EvaluationResult]])] = unique_elements.map(matcher => (matcher, base_matcher_results.map { round_result => round_result.get(matcher)}))


    val max_size_of_matchings = result_per_matcher.maxBy(tuple => {
      tuple._2.filter(_.isDefined).size
    })._2.filter(_.isDefined).size


    val filtered_results_per_matcher = result_per_matcher.filter(tuple => {
      tuple._2.filter(_.isDefined).size >= max_size_of_matchings
    })


    // filter results that were used in every matcher

    //now aggregate for each matcher
    val aggregated_result_per_matcher: Map[String, AggregatedEvaluationResult] = filtered_results_per_matcher.map { case (matcher_name, eval_results) => (matcher_name, EvaluationResultAggregator.aggregateEvaluationResultsOption(eval_results))}.toMap

    aggregated_result_per_matcher
  }

}

package de.unima.dws.oamatching.pipeline.optimize


import de.unima.dws.oamatching.core.matcher.Matcher
import de.unima.dws.oamatching.core.{Alignment, AggregatedEvaluationResult, EvaluationResultAggregator}
import de.unima.dws.oamatching.core.matcher.{Matcher, ElementLevelMatcher}
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.{MatchingPipelineCore, MatchingProblem, FeatureVector}
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationMatchingRunner, EvaluationMatchingTask}
import de.unima.dws.oamatching.pipeline.util.MetaDataMgmt

import scala.collection.JavaConverters._
import scala.collection.immutable.NumericRange.Exclusive
import scala.collection.immutable.{NumericRange, Map}
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.mutable

/**
 * Created by mueller on 28/01/15.
 */
object ParameterOptimizer extends App {
  val start = System.currentTimeMillis()
  MatcherRegistry.init
  val problems = EvaluationMatchingRunner.parseConference("ontos/2014/conference")
  //optimizeAdvancedPipeline("conference",problems, List(0.3, 0.4))

 //val test_matcher = MatcherRegistry.getMatcherByName("umbcphrasesim").get

  //val test = optimizeSingleThresholdElementLevelMatcher(test_matcher,problems,getDoubleGrid(0.5,0.999,4))
  val test = optimizeThresholdForAllBaseMatcher("conference", problems,getDoubleGrid(0.2,0.99,10))

  val totaltime = System.currentTimeMillis() - start

  println(totaltime)

println(test)
  //def optimizeAdvancedPipeline =  optimizeThresholdPipeline(MatchingPipelineCore.matchProblem)_

  /**
   *
   * @param task_name
   * @param problems
   * @param thresholds
   * @return
   */
  def optimizeThresholdForAllBaseMatcher(task_name: String, problems: Seq[EvaluationMatchingTask], thresholds: List[Double]): Map[String, Double] = {

    val optimized: ParMap[String, Double] = MatcherRegistry.matcher_by_name.par.map { case (name, matcher) => {
      println("Optimize for " + name)
      val res  = (name, optimizeSingleThresholdElementLevelMatcher(matcher, problems, thresholds))
      println("Optimization done for  " + name)
      res
    }
    }.toMap

    println("Now store results")
    storeThresholds(task_name, optimized.seq)

    optimized.seq
  }

  /**
   *
   * @param matcher
   * @param problems
   * @param thresholds
   */
  def optimizeSingleThresholdElementLevelMatcher(matcher: Matcher, problems: Seq[EvaluationMatchingTask], thresholds: List[Double]): Double = {

    val optimization_result = thresholds.map(threshold => (threshold, optimizeSingleRound(matcher, problems, threshold)))

    val best_result = optimization_result.maxBy(result => result._2.micro_eval_res.f1Measure)
    //return best threshold
    best_result._1
  }

  def optimizeSingleRound(matcher: Matcher, problems: Seq[EvaluationMatchingTask], threshold: Double): AggregatedEvaluationResult = {

    val results = problems.map(task => {
      val alignment = matcher.align(task.matching_problem, threshold)
      val res = alignment.evaluate(task.reference)

      res
    });

    val agg_res = EvaluationResultAggregator.aggregateEvaluationResults(results.toList)

    agg_res
  }

  def storeThresholds(data_set: String, thresholds: Map[String, Double]): Unit = {
    for ((matcher, threshold) <- thresholds) {
      MetaDataMgmt.storeThreshold(data_set, matcher, threshold)
    }
  }


  /**
   * Optimizes for a given pipeline function the matchings selection threshold, in future there will be one after another parameter optimization strategy
   * @param pipelineFct core pipeline, performing meta matching
   * @param problems set of problems to be solved
   * @param threshold the threshold
   * @return
   */
  def optimizeSingleRoundPipeline(pipelineFct: (MatchingProblem,Map[String,Double])=> (Alignment,FeatureVector) )( problems: Seq[EvaluationMatchingTask], threshold: Double): AggregatedEvaluationResult = {
    println("optimize round")
    val results = problems.map(task => {
      val param_map: Map[String,Double] = Map( ("threshold",threshold))
      val pipeline_res = pipelineFct(task.matching_problem, param_map)

      val alignment =  pipeline_res._1
      val res = alignment.evaluate(task.reference)
      res
    });

    EvaluationResultAggregator.aggregateEvaluationResults(results.toList)
  }

  /**
   *
   * @param pipelineFct
   * @param problems
   * @param thresholds
   * @return
   */
  def optimizeThresholdPipeline(pipelineFct: (MatchingProblem,Map[String,Double])=> (Alignment,FeatureVector) ) (data_set:String,problems: Seq[EvaluationMatchingTask], thresholds: List[Double]): Double = {

    val optimization_result = thresholds.map(threshold => (threshold, optimizeSingleRoundPipeline(pipelineFct)( problems, threshold)))

    val best_result = optimization_result.maxBy(result => result._2.micro_eval_res.f1Measure)

    //store result
    MetaDataMgmt.storeThreshold(data_set,"pipeline",best_result._1)
    //return best threshold
    best_result._1
  }


  //######################## some util funcitons

  def getDoubleGrid(val_from:Double,val_to:Double, size:Int):List[Double] = {
    val step_size: Double = (val_to - val_from) / size.toDouble

    val test =  val_from to val_to by step_size
    //val test: Exclusive[Double] = NumericRange[Double](val_from, val_to,step_size)
    println(test)
    test.toList
  }


}
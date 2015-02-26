package de.unima.dws.oamatching.thesis

import java.io.File

import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.core.{AggregatedEvaluationResult, AlignmentParser, MatchRelation}
import de.unima.dws.oamatching.pipeline.evaluation.EvaluationMatchingRunner
import de.unima.dws.oamatching.pipeline.optimize.ParameterOptimizer
import de.unima.dws.oamatching.pipeline.{MatchingPipelineCore, ScoreNormalizationFunctions}

import scala.collection.immutable.Map

/**
 * Created by mueller on 18/02/15.
 */
object FileBasedThresholdOptimization extends App {
  RapidminerJobs.init()
  /*analyseAndEvaluate("/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/oacode_lof_regular.rmp", ParameterOptimizer.getDoubleGrid(0.95, 0.999999999, 10))

  def analyseAndEvaluate(rapidminer_file: String, thresholds: Seq[Double]): Unit = {
    val pairs: List[(File, File)] = MiscExperiments.getListOfMatchingRefPairs

    val results: Map[Double, AggregatedEvaluationResult] = thresholds.map(threshold => {
      val eval_res = pairs.map { case (ref_file, matching_file) => {
        val ref_alingment = AlignmentParser.parseRDF(ref_file)
        val result: (Int, Map[String, (Double, Double)],Map[MatchRelation, Double]) = RapidminerJobs.rapidminerOutlierDetectionExperiments(rapidminer_file, matching_file,null)
        val alignment = MatchingPipelineCore.postProcessMatchings(ScoreNormalizationFunctions.normalizeByGaussianScaling, threshold, result)

        val res = alignment.evaluate(ref_alingment)
        println(res)
        res
      }
      }.toList
      val agg_res=  EvaluationMatchingRunner.computeAggregatedResults(eval_res)

      println("F-Measure " +agg_res.macro_eval_res.f1Measure)
      threshold ->agg_res
    }).toMap


    val best_res = results.maxBy { case (treshold, aggregated_result) => {
      aggregated_result.macro_eval_res.f1Measure
    }
    }

    println("Best Threshold: " + best_res._1)

    println("Result: " + best_res._2)
  }
*/
}

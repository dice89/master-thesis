package de.unima.dws.oamatching.thesis

import java.io.File

import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.pipeline.evaluation.EvaluationMatchingRunner
import de.unima.dws.oamatching.pipeline.optimize.ParameterOptimizer
import de.unima.dws.oamatching.pipeline.{MatchingSelector, ScoreNormalizationFunctions}
import org.apache.commons.math.stat.descriptive.moment.Mean
import org.apache.commons.math.stat.inference.ChiSquareTestImpl
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest
import org.jfree.chart.JFreeChart

/**
 * Created by mueller on 18/02/15.
 */

case class OutlierEvalStatisticsObject(ds_name: String, stdev: Double, mean: Double, p_value: Double, top_outlier_score: Double, min_outlier_score: Double, top_10_outlier_score: List[Double], eval_at_top_k: EvaluationResult)


object CreateOutlierScoreStatistics extends App {
  RapidminerJobs.init()
  val mean_computer = new Mean()
  val stdev_computer = new StandardDeviation()

  val outlier_processes_for_eval = List("/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/oacode_cblof_unweighted_regular.rmp")


  /*val outlier_processes_for_eval = List("/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/remove_corr_pre/oacode_loop_2.rmp",
    "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/remove_corr_pre/oacode_lof.rmp",
    "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/remove_corr_pre/oacode_lof_regular.rmp",
    "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/remove_corr_pre/oacode_cblof_unweighted.rmp",
    "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/remove_corr_pre/oacode_cblof_unweighted_regular.rmp",
    "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/remove_corr_pre/oacode_ldcof.rmp",
    "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/remove_corr_pre/oacode_ldcof_regular.rmp",
    "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/remove_corr_pre/oacode_cblof_unweighted_regular_db_scan.rmp")*/

  /*val outlier_processes_for_eval = List("/Users/mueller/Documents/master-thesis/RapidminerRepo/oacode_loop_2.rmp",
     "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/oacode_lof.rmp",
     "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/oacode_lof_regular.rmp",
     "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/oacode_cblof_unweighted.rmp",
     "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/oacode_cblof_unweighted_regular.rmp",
     "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/oacode_ldcof.rmp",
     "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/oacode_ldcof_regular.rmp",
     "/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/oacode_cblof_unweighted_regular_db_scan.rmp")*/


  computeAndVisualizeOutlierScoresAll(outlier_processes_for_eval)


  /**
   * Calls the outlier detections script and plotts the outlier distribution in a histogram
   * @param rapidminer_file
   */
  def computeAndVisualizeOutlierScores(rapidminer_file: String): Unit = {
    val pairs: List[(File, File)] = MiscExperiments.getListOfMatchingRefPairs

    val technique_name = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.lastIndexOf("."));


    val chart_data: List[(Seq[(JFreeChart, JFreeChart)], OutlierEvalStatisticsObject, Seq[Seq[(MatchRelation, Double)]], Alignment, Alignment, Map[String, (Map[MatchRelation, Double], Alignment)])] = pairs.map { case (ref_file, matching_file) => {

      val result: (Int, Map[MatchRelation, Double]) = RapidminerJobs.rapidminerOutlierDetectionExperiments(rapidminer_file, matching_file)

      //compute statistics
      val values = result._2.values.toArray

      //normalizes values
      val norm_res_gaussian: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGaussianScaling(result._1, result._2).toMap
      val norm_res_euclidean_max: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByMaxEuclideanDistance(result._1, result._2).toMap
      val norm_res_gamma: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGammaScaling(result._1, result._2).toMap
      val norm_res_znorm: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByZScore(result._1, result._2).toMap
      val ref_alignment = AlignmentParser.parseRDF(ref_file)
      val resulting_matchings: Map[String, (Map[MatchRelation, Double], Alignment)] = Map(("none", (result._2, ref_alignment)), ("gaussian", (norm_res_gaussian, ref_alignment)), ("zscore", (norm_res_znorm, ref_alignment)), ("gammma", (norm_res_gamma, ref_alignment)), ("euclidean_max", (norm_res_euclidean_max, ref_alignment)))

      val n = 20
      val top_n: Seq[(MatchRelation, Double)] = getTopNResults(result._2, n)
      val top_n_gaussian: Seq[(MatchRelation, Double)] = getTopNResults(norm_res_gaussian, n)
      val top_n_euclidean: Seq[(MatchRelation, Double)] = getTopNResults(norm_res_euclidean_max, n)
      val top_n_gamma: Seq[(MatchRelation, Double)] = getTopNResults(norm_res_gamma, n)
      val top_n_z: Seq[(MatchRelation, Double)] = getTopNResults(norm_res_znorm, n)
      val top_scores = Seq(top_n, top_n_gaussian, top_n_euclidean, top_n_gamma, top_n_z)


      //produce alignment out of top 10

      val top_n_alignment = new Alignment("", "", top_n.toMap)


      val eval_res = top_n_alignment.evaluate(ref_alignment)

      val name = matching_file.getName.slice(0, matching_file.getName.lastIndexOf("."));

      //create charts
      val charts_unnormalized = HistogramChartFactory.createHistogramForOutlierScores(result._2, name + " unnormalized ")
      val charts_normalized = HistogramChartFactory.createHistogramForOutlierScores(norm_res_gaussian, name + " gaussian scaled")
      val charts_euclideand_normalized = HistogramChartFactory.createHistogramForOutlierScores(norm_res_euclidean_max, name + " euclidean max scaled")
      val charts_gamma_normalized = HistogramChartFactory.createHistogramForOutlierScores(norm_res_gamma, name + " gamma scaled")
      val charts_z_normalized = HistogramChartFactory.createHistogramForOutlierScores(norm_res_znorm, name + " zscore scaled")

      val charts = Seq(charts_unnormalized, charts_normalized, charts_euclideand_normalized, charts_gamma_normalized, charts_z_normalized)

      //compute statistics
      val statistics: OutlierEvalStatisticsObject = computeBaseStatisticsOverOutlierScores(values, name, eval_res)
      (charts, statistics, top_scores, ref_alignment, top_n_alignment, resulting_matchings)
    }
    }

    val matching_results: List[Map[String, (Map[MatchRelation, Double], Alignment)]] = chart_data.map(_._6)

    val optimization_grid = ParameterOptimizer.getDoubleGrid(0.001, 0.9999999999, 1000)

    val threshold_optimized_values: (Map[String, Seq[(Double, AggregatedEvaluationResult)]], Map[String, (Double, AggregatedEvaluationResult)], Map[String, AggregatedEvaluationResult], Map[String, Seq[(Double, EvaluationResult)]]) = findOptimalThresholds(matching_results, optimization_grid)
    val table_data = (threshold_optimized_values._2, threshold_optimized_values._3, threshold_optimized_values._4)
    val precision_recall_curve_data = threshold_optimized_values._1
    HistogramChartFactory.storeMultipleChartsIntoPDF(chart_data.toSeq, s"thesisexperiments/outliereval/$technique_name.pdf",table_data,precision_recall_curve_data )
  }

  def getTopNResults(result: Map[MatchRelation, Double], n: Int): Seq[(MatchRelation, Double)] = {
    val res_sorted = result.toSeq.sortWith((tuple1, tuple2) => tuple1._2 > tuple2._2)
    val top = res_sorted.zipWithIndex.filter(zipped_tuple => zipped_tuple._2 <= n).unzip._1
    top
  }

  /**
   * Find Local and global optima
   * @param scores_by_norm_technique
   * @param threshold_grid
   * @return
   */
  def findOptimalThresholds(scores_by_norm_technique: List[Map[String, (Map[MatchRelation, Double], Alignment)]], threshold_grid: List[Double]):
  (Map[String, Seq[(Double, AggregatedEvaluationResult)]] ,Map[String, (Double, AggregatedEvaluationResult)], Map[String, AggregatedEvaluationResult], Map[String, Seq[(Double, EvaluationResult)]]) = {
    val unique_techniques = scores_by_norm_technique.head.keys.toVector

    val results_by_techniques: Map[String, List[(Map[MatchRelation, Double], Alignment)]] = unique_techniques.map(technique => {
      //get results for a techniques
      val matchings_for_technique: List[(Map[MatchRelation, Double], Alignment)] = scores_by_norm_technique.map(elem => elem.get(technique).get)

      (technique, matchings_for_technique)
    }).toMap

    results_by_techniques.foreach(elem => println(elem._2.size))

    //optimize for each matching technique and find global optimum
    val global_results: Map[String, Seq[(Double, AggregatedEvaluationResult)]] = results_by_techniques.map { case (name, list_of_matchings) => {

      //try for all thresholds
      val results_by_threshold = threshold_grid.map(threshold => {
        val eval_res_single_list: Seq[EvaluationResult] = list_of_matchings.map(single_matchings => {
          val selected = MatchingSelector.fuzzyGreedyRankSelectorSimple(single_matchings._1, threshold,1.03)
          val alignment = new Alignment(null, null, selected)

          alignment.evaluate(single_matchings._2)
        })
        val agg_res = EvaluationMatchingRunner.computeAggregatedResults(eval_res_single_list.toList)
        (threshold, agg_res)
      })
      (name, results_by_threshold)
    }
    }.toMap

    //get best result
    val best_global_results: Map[String, (Double, AggregatedEvaluationResult)] = global_results.map{ case (name, list_of_results) => {
      val best_result: (Double, AggregatedEvaluationResult) = list_of_results.maxBy(_._2.macro_eval_res.f1Measure)
      (name, best_result)
    }}

    //find local optima, so for each technique and for each matched dataset the best threshold and result
    val best_local_results: Map[String, Seq[(Double, EvaluationResult)]] = results_by_techniques.map { case (name, list_of_matchings) => {
      name -> list_of_matchings.map(single_matchings => {
        //find best threshold
        val res_by_threshold: Seq[(Double, EvaluationResult)] = threshold_grid.map(threshold => {
          val selected = MatchingSelector.fuzzyGreedyRankSelectorSimple(single_matchings._1, threshold,1.03)
          val alignment = new Alignment(null, null, selected)

          (threshold, alignment.evaluate(single_matchings._2))
        });
        // find best by f-measure
        val best_result = res_by_threshold.maxBy(_._2.f1Measure)

        best_result;
      })
    }
    }.toMap

    //aggregate best_local_results
    val results_local_optima: Map[String, AggregatedEvaluationResult] = best_local_results.map { case (name, list_of_eval_results) => {

      name -> EvaluationMatchingRunner.computeAggregatedResults(list_of_eval_results.map(_._2).toList)
    }
    }

    (global_results,best_global_results, results_local_optima, best_local_results)
  }

  /**
   * Calls the single computations and viszualisation routine for all base datasets
   * @param rapidminer_files
   */
  def computeAndVisualizeOutlierScoresAll(rapidminer_files: List[String]): Unit = {
    rapidminer_files.foreach(computeAndVisualizeOutlierScores(_))
  }

  /**
   * Computes basic statistics over the  outlier scores
   * @param values
   * @param name
   */
  def computeBaseStatisticsOverOutlierScores(values: Array[Double], name: String, evaluationResult: EvaluationResult): OutlierEvalStatisticsObject = {

    val stdev = stdev_computer.evaluate(values)
    val mean = mean_computer.evaluate(values)

    val values_sorted = values.sortWith(_ > _).zipWithIndex

    val top_values = values_sorted.filter(tuple => tuple._2 <= 10).toList

    val min_value = values.minBy(value => value)

    val null_hypothesis = new NormalDistribution(mean, stdev)

    val hypo_test = new KolmogorovSmirnovTest()

    val p_value = hypo_test.kolmogorovSmirnovTest(null_hypothesis, values, false) / 2
    val chi_square_test = new ChiSquareTestImpl()

    OutlierEvalStatisticsObject(name, stdev, mean, p_value, values_sorted.head._1, min_value, top_values.unzip._1, evaluationResult)
  }

}

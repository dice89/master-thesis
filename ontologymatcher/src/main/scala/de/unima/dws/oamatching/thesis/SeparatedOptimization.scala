package de.unima.dws.oamatching.thesis

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.oamatching.analysis.{RapidminerJobs, SeparatedResults}
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.pipeline.ScoreNormalizationFunctions
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationMatchingRunner, EvaluationMatchingTask}
import de.unima.dws.oamatching.pipeline.optimize.ParameterOptimizer

import scala.collection.immutable.Map

/**
 * Created by mueller on 27/02/15.
 */
trait SeparatedOptimization extends ResultServerHandling with LazyLogging with OptimizationDebugging {


  def executeProcessSeparated(ds_name: String, run_number: Int, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], ref_matching_pairs: List[(EvaluationMatchingTask, File)], rapidminer_file: String, pre_pro_key: String, parameters: Map[String, Map[String, Double]], processes: Map[String, String]): ProcessEvalExecutionResultNonSeparated = {

    val process_name = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.lastIndexOf("."));
    val process_name_with_ending = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.size);
    //val process_type: String = IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS.get(process_name_with_ending).get
    val process_type: String = processes.get(process_name_with_ending).get


    val normalized_per_category: List[(Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Alignment)] = ref_matching_pairs.par.map { case (ref_file, matching_file) => {

      val ref_alignment: Alignment = ref_file.reference

      //build different reference alignments out of original one for different classes
      val classes_alignment = new Alignment(null, null, ref_alignment.correspondences.filter(_.owl_type.equals(Cell.TYPE_CLASS)).toList)
      val op_alignment = new Alignment(null, null, ref_alignment.correspondences.filter(_.owl_type.equals(Cell.TYPE_OBJECT_PROPERTY)).toList)
      val dp_alignment = new Alignment(null, null, ref_alignment.correspondences.filter(_.owl_type.equals(Cell.TYPE_DT_PROPERTY)).toList)

      val result: SeparatedResults = RapidminerJobs.rapidminerOutlierDetectionExperimentsSeparated(run_number, rapidminer_file, matching_file, parameters, pre_pro_key, process_type)

      val class_normalized = getNormalizedScores(classes_alignment, result.class_matchings)
      val dp_normalized = getNormalizedScores(dp_alignment, result.dp_matchings)
      val op_normalized = getNormalizedScores(op_alignment, result.op_matchings)

      (class_normalized, dp_normalized, op_normalized, (ref_alignment))
    }
    }.toList


    val optimization_grid = ParameterOptimizer.getDoubleGrid(0.001, 1.1, 1000)
    val optimal_thresholds = findOptimalThresholds(selection_function, normalized_per_category, optimization_grid)

    logger.info(s"Start threshold optimization for $ds_name and $process_name in run $run_number")

    //construct final matchings and evaluate
    val best_results = optimal_thresholds.map { case (norm_technique, (class_threshold, dp_threshold, op_threshold)) => {
      val eval_res = evaluateRound(norm_technique, selection_function, normalized_per_category, class_threshold, dp_threshold, op_threshold,true)
      println(eval_res)
      norm_technique -> eval_res
    }
    }


    val best_result = best_results.maxBy(res_by_norm => res_by_norm._2.result.macro_eval_res.f1Measure)

    val json_result = createJSONResultString(ds_name, process_type, pre_pro_key, true, best_result._2.result, parameters, createJSONThresholdStringSeparated(best_result._2))

    sendResultToServer(json_result)
    logger.info(s"Done with threshold optimization for $ds_name and $process_name in run $run_number")
    ProcessEvalExecutionResultNonSeparated(true, best_result._2.result, null, null, null, null, best_result, best_results)

  }

  /**
   *
   * @param ref_alignment
   * @param test
   * @return
   */
  def getNormalizedScores(ref_alignment: Alignment, test: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double])): Map[String, Map[MatchRelation, Double]] = {
    val norm_res_gaussian: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGaussianScaling(test._1, test._2, test._3).toMap
    val norm_res_euclidean_max: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByMaxEuclideanDistance(test._1, test._2, test._3).toMap
    val norm_res_gamma: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGammaScaling(test._1, test._2, test._3).toMap
    val norm_res_znorm: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByZScore(test._1, test._2, test._3).toMap

    val resulting_matchings: Map[String, Map[MatchRelation, Double]] = Map(("none", test._3), ("gaussian", norm_res_gaussian), ("zscore", norm_res_znorm), ("gammma", norm_res_gamma), ("euclidean_max", norm_res_euclidean_max))

    resulting_matchings
  }


  /**
   *
   * @param selection_function
   * @param class_matchings
   * @param dp_matchings
   * @param op_matchings
   * @param class_threshold
   * @param dp_threshold
   * @param op_threshold
   * @param ref_alignment
   * @return
   */
  def selectAndEvaluate(selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], class_matchings: Map[MatchRelation, Double], dp_matchings: Map[MatchRelation, Double], op_matchings: Map[MatchRelation, Double], class_threshold: Double, dp_threshold: Double, op_threshold: Double, ref_alignment: Alignment, debug: Boolean): EvaluationResult = {

    val unselected = class_matchings ++ dp_matchings ++ op_matchings

    val selected_classes: Map[MatchRelation, Double] = selection_function(class_matchings, class_threshold)
    val selected_dps: Map[MatchRelation, Double] = selection_function(dp_matchings, dp_threshold)
    val selected_ops: Map[MatchRelation, Double] = selection_function(op_matchings, op_threshold)

    val selected: Map[MatchRelation, Double] = selected_classes ++ selected_dps ++ selected_ops

    val result = if (debug) {
      debugAndEvaluateSeparated(class_threshold, dp_threshold, op_threshold, unselected, ref_alignment, selected, "", false)

    } else {
      val alignment = new Alignment(null, null, selected)

      alignment.evaluate(ref_alignment)
    }

    result
  }


  def evaluateRound(norm_technique: String, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], normalizedScores: List[(Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Alignment)], class_threshold: Double, dp_threshold: Double, op_threshold: Double, debug: Boolean): SeparatedResult = {

    val unique_techniques = normalizedScores.head._1.keys.toVector

    //get single results
    val results: List[EvaluationResult] = normalizedScores.map { case (class_matchings, dp_matchings, op_matchings, ref_alignment) => {

      val class_matchings_norm = class_matchings.get(norm_technique).get
      val dp_matchings_norm = dp_matchings.get(norm_technique).get
      val op_matchings_norm = op_matchings.get(norm_technique).get

      selectAndEvaluate(selection_function, class_matchings_norm, dp_matchings_norm, op_matchings_norm, class_threshold, dp_threshold, op_threshold, ref_alignment, debug)

    }
    }.toList

    //get result list by norm

    val agg_res = EvaluationMatchingRunner.computeAggregatedResults(results)
    SeparatedResult(class_threshold, dp_threshold, op_threshold, agg_res)

  }


  /**
   *
   * @param selection_function
   * @param normalizedScores
   * @param threshold_grid
   * @return
   */
  def findOptimalThresholds(selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], normalizedScores: List[(Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Alignment)], threshold_grid: List[Double]): Map[String, (Double, Double, Double)] = {

    val unique_techniques = normalizedScores.head._2.keys.toVector
    //optimize thresholds


    //first find best class threshold

    val best_class_thresholds_by_norm: Map[String, Double] = unique_techniques.map(norm_technique => {
      val results_by_threshold = threshold_grid.map(threshold => {
        (threshold, evaluateRound(norm_technique, selection_function, normalizedScores, threshold, 1.1, 1.1, false))
      })
      val best_result = results_by_threshold.maxBy(_._2.result.macro_eval_res.f1Measure)

      norm_technique -> best_result._1
    }).toMap

    //find best dp threshold
    val best_dp_thresholds_by_norm: Map[String, Double] = unique_techniques.map(norm_technique => {
      val class_threshold = best_class_thresholds_by_norm.get(norm_technique).get
      val results_by_threshold = threshold_grid.map(threshold => {

        (threshold, evaluateRound(norm_technique, selection_function, normalizedScores, class_threshold, threshold, 1.1, false))
      })
      val best_result = results_by_threshold.maxBy(_._2.result.macro_eval_res.f1Measure)

      norm_technique -> best_result._1
    }).toMap


    val best_op_thresholds_by_norm: Map[String, Double] = unique_techniques.map(norm_technique => {
      val class_threshold = best_class_thresholds_by_norm.get(norm_technique).get
      val dp_threshold = best_dp_thresholds_by_norm.get(norm_technique).get
      val results_by_threshold = threshold_grid.map(threshold => {

        (threshold, evaluateRound(norm_technique, selection_function, normalizedScores, class_threshold, dp_threshold, threshold, false))
      })
      val best_result = results_by_threshold.maxBy(_._2.result.macro_eval_res.f1Measure)

      norm_technique -> best_result._1
    }).toMap


    //get best thresholds by norm technique
    val best_by_norm_technique: Map[String, (Double, Double, Double)] = unique_techniques.map(norm_technique => {
      val best_class = best_class_thresholds_by_norm.get(norm_technique).get
      val best_dp = best_dp_thresholds_by_norm.get(norm_technique).get
      val best_op = best_op_thresholds_by_norm.get(norm_technique).get

      norm_technique ->(best_class, best_dp, best_op)
    }).toMap

    best_by_norm_technique
  }


  /**
   *
   * @param selection_function
   * @param scores_by_norm_technique
   * @param threshold_grid
   * @return
   */
  def findOptimalThresholdGlobalOnly(selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], scores_by_norm_technique: List[Map[String, (Map[MatchRelation, Double], Alignment)]], threshold_grid: List[Double]): ThresholdOptResult = {
    val unique_techniques = scores_by_norm_technique.head.keys.toVector

    val results_by_techniques = unique_techniques.par.map(technique => {
      //get results for a techniques
      val matchings_for_technique: List[(Map[MatchRelation, Double], Alignment)] = scores_by_norm_technique.map(elem => elem.get(technique).get)

      (technique, matchings_for_technique)
    }).toMap

    //optimize for each matching technique and find global optimum
    val global_results = results_by_techniques.map { case (name, list_of_matchings) => {

      //try for all thresholds
      val results_by_threshold: List[(Double, AggregatedEvaluationResult)] = threshold_grid.map(threshold => {
        val eval_res_single_list: Seq[EvaluationResult] = list_of_matchings.map(single_matchings => {
          // val selected = MatchingSelector.fuzzyGreedyRankSelectorDelta(single_matchings._1, threshold, delta_fuzzy_selection)
          val selected = selection_function(single_matchings._1, threshold)

          val alignment = new Alignment(null, null, selected)

          alignment.evaluate(single_matchings._2)
        })
        val agg_res = EvaluationMatchingRunner.computeAggregatedResults(eval_res_single_list.toList)
        (threshold, agg_res)
      })
      (name, results_by_threshold)
    }
    }.toMap

    val best_global_results = global_results.map { case (name, list_of_results) => {
      val best_result: (Double, AggregatedEvaluationResult) = list_of_results.maxBy(_._2.macro_eval_res.f1Measure)


      //handle edge case best global result in terms of f-measure is 0.0 -> then take that result that minimized the fp
      if (best_result._2.macro_eval_res.f1Measure == 0.00) {
        val edge_case_best_result = list_of_results.minBy(_._2.micro_eval_res.precision)
        (name, edge_case_best_result)
      } else {
        (name, best_result)
      }
    }
    }


    ThresholdOptResult(global_results.seq, best_global_results.seq, null, null)
  }


}

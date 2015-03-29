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


    val normalized_per_category = ref_matching_pairs.par.map { case (ref_file, matching_file) => {

      val ref_alignment: Alignment = ref_file.reference

      //build different reference alignments out of original one for different classes
      val classes_alignment = new Alignment(null, null, ref_alignment.correspondences.filter(_.owl_type.equals(Cell.TYPE_CLASS)).toList)
      val op_alignment = new Alignment(null, null, ref_alignment.correspondences.filter(_.owl_type.equals(Cell.TYPE_OBJECT_PROPERTY)).toList)
      val dp_alignment = new Alignment(null, null, ref_alignment.correspondences.filter(_.owl_type.equals(Cell.TYPE_DT_PROPERTY)).toList)

      val result: SeparatedResults = RapidminerJobs.rapidminerOutlierDetectionExperimentsSeparated(run_number, rapidminer_file, matching_file, parameters, pre_pro_key, process_type)

      val class_normalized = getNormalizedScores(classes_alignment, result.class_matchings)
      val dp_normalized = getNormalizedScores(dp_alignment, result.dp_matchings)
      val op_normalized = getNormalizedScores(op_alignment, result.op_matchings)

      ((class_normalized, dp_normalized, op_normalized), (ref_alignment))
    }
    }.toList

    val tuple_wise: (List[Map[String, (Map[MatchRelation, Double], Alignment)]], List[Map[String, (Map[MatchRelation, Double], Alignment)]], List[Map[String, (Map[MatchRelation, Double], Alignment)]]) = normalized_per_category.unzip._1.unzip3

    logger.info(s"Start threshold optimization for $ds_name and $process_name in run $run_number")



    val optimization_grid = ParameterOptimizer.getDoubleGrid(0.001, 1.1, 1000)
    //globally normalize each tuple
    val best_threshold_classes = findOptimalThresholdGlobalOnly(selection_function, tuple_wise._1, optimization_grid)
    val best_threshold_dps = findOptimalThresholdGlobalOnly(selection_function, tuple_wise._2, optimization_grid)
    val best_threshold_ops = findOptimalThresholdGlobalOnly(selection_function, tuple_wise._3, optimization_grid)

    //construct final matchings and evaluate
    val best_results: Map[String, SeparatedResult] = best_threshold_classes.best_global_results.keys.map(norm_technique => {
      val class_threshold = best_threshold_classes.best_global_results.get(norm_technique).get._1
      val dp_threshold = best_threshold_dps.best_global_results.get(norm_technique).get._1
      val op_threshold = best_threshold_ops.best_global_results.get(norm_technique).get._1


      val eased_thresholds = List((class_threshold - 0.01, dp_threshold - 0.01, op_threshold - 0.01), (class_threshold - 0.025, dp_threshold - 0.025, op_threshold - 0.025), (class_threshold - 0.05, dp_threshold - 0.05, op_threshold - 0.05))
      val eased_thresholds_results = eased_thresholds.map(triple => {

        val results = normalized_per_category.map { case ((classes_matchings, dp_matchings, op_matchings), ref_align) => {

          val all_matchings_init = classes_matchings ++ dp_matchings ++ op_matchings
          val all_matchings = all_matchings_init.get(norm_technique).get._1

          val selected_classes: Map[MatchRelation, Double] = selection_function(classes_matchings.get(norm_technique).get._1, triple._1)
          val selected_dps: Map[MatchRelation, Double] = selection_function(dp_matchings.get(norm_technique).get._1, triple._2)
          val selected_ops: Map[MatchRelation, Double] = selection_function(op_matchings.get(norm_technique).get._1, triple._3)

          val all_selected_matchings_list = selected_classes ++ selected_dps ++ selected_ops

          val eval_res = debugAndEvaluate(op_threshold, all_matchings, ref_align, all_selected_matchings_list, norm_technique,true)



          eval_res
        }
        }.toList


        val agg_res = EvaluationMatchingRunner.computeAggregatedResults(results)
        println(s"Result for $triple")
        println(agg_res)
        norm_technique -> SeparatedResult( triple._1,  triple._2, triple._3, agg_res)

      })


      eased_thresholds_results.maxBy(_._2.result.macro_eval_res.f1Measure)
    }).toMap




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
  def getNormalizedScores(ref_alignment: Alignment, test: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double])): Map[String, (Map[MatchRelation, Double], Alignment)] = {
    val norm_res_gaussian: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGaussianScaling(test._1, test._2, test._3).toMap
    val norm_res_euclidean_max: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByMaxEuclideanDistance(test._1, test._2, test._3).toMap
    val norm_res_gamma: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGammaScaling(test._1, test._2, test._3).toMap
    val norm_res_znorm: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByZScore(test._1, test._2, test._3).toMap

    val resulting_matchings: Map[String, (Map[MatchRelation, Double], Alignment)] = Map(("none", (test._3, ref_alignment)), ("gaussian", (norm_res_gaussian, ref_alignment)), ("zscore", (norm_res_znorm, ref_alignment)), ("gammma", (norm_res_gamma, ref_alignment)), ("euclidean_max", (norm_res_euclidean_max, ref_alignment)))

    resulting_matchings
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

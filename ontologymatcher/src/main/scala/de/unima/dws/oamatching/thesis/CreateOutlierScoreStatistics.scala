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

import scala.collection.immutable.Map

/**
 * Created by mueller on 18/02/15.
 */


/**
 * Case class that contains basic info of the outlier evaluation run
 * @param ds_name
 * @param stdev
 * @param mean
 * @param p_value
 * @param top_outlier_score
 * @param min_outlier_score
 * @param top_10_outlier_score
 */
case class OutlierEvalStatisticsObject(ds_name: String, stdev: Double, mean: Double, p_value: Double, top_outlier_score: Double, min_outlier_score: Double, top_10_outlier_score: List[Double])

/**
 * Case class to store the results of a threshold optimization, might not contain local optima
 * @param global_results_per_threshold
 * @param best_global_results
 * @param local_optima_per_ds
 * @param agg_local_optimum
 */
case class ThresholdOptResult(global_results_per_threshold: Map[String, Seq[(Double, AggregatedEvaluationResult)]], best_global_results: Map[String, (Double, AggregatedEvaluationResult)], local_optima_per_ds: Map[String, Seq[(Double, EvaluationResult)]], agg_local_optimum: Map[String, AggregatedEvaluationResult])

case class ProcessEvalExecutionResultNonSeparated(separated: Boolean, best_agg_res: AggregatedEvaluationResult, local_global_threshold: ThresholdOptResult, statistics: List[OutlierEvalStatisticsObject], top_n_results: Map[String, Map[String, Seq[(MatchRelation, Double, Boolean)]]], best_result: (String, (Double, AggregatedEvaluationResult)), best_separated_result: (String, SeparatedResult), best_separated_results: Map[String, SeparatedResult])

/**
 * Case class representing a separated outlier analysis result
 * @param c_threshold
 * @param dp_threshold
 * @param op_threshold
 * @param result
 */
case class SeparatedResult(c_threshold: Double, dp_threshold: Double, op_threshold: Double, result: AggregatedEvaluationResult)

case class ProcessEvalExecutionResultsNonSeparated(separated: Boolean, overall_agg_best: AggregatedEvaluationResult, best_result: (String, ProcessEvalExecutionResultNonSeparated), results: Map[String, ProcessEvalExecutionResultNonSeparated])

object CreateOutlierScoreStatistics extends App with OutlierEvaluationProcessParser with SeparatedOptimization {
  RapidminerJobs.init()

  /*########################################################################
                         Algorithms
    ########################################################################*/
  val IMPLEMENTED_OUTLIER_METHODS_BY_NAME = Map(
    "rll_m5p" -> "oacode_rll_m5p.rmp",
    "rll_iso" -> "oacode_rll_iso.rmp",
    "rnn" -> "oacode_rnn.rmp",
    "cblof_regular_db" -> "oacode_cblof_unweighted_regular_db_scan.rmp",
    "knn" -> "oacode_knn.rmp",
    "ldcof_regular_x_means" -> "oacode_ldcof_regular_x_means.rmp",
    "ldcof_regular_db_scan" -> "oacode_ldcof_regular_db_scan.rmp",
    "lcdof_x_means" -> "oacode_ldcof_x_means.rmp",
    "lof_regular" -> "oacode_lof_regular.rmp",
    "lof" -> "oacode_lof.rmp",
    "loop" -> "oacode_loop.rmp",
    "cblof_regular_x_means" -> "oacode_cblof_unweighted_regular_x_means.rmp",
    "cblof_x_means" -> "oacode_cblof_unweighted_x_means.rmp"
  )
  
  //    "lof_regular" -> "oacode_lof_regular.rmp",
 // "cblof_x_means" -> "oacode_cblof_unweighted_x_means.rmp"

  val IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS: Map[String, String] = IMPLEMENTED_OUTLIER_METHODS_BY_NAME.map(tuple => (tuple._2, tuple._1))

  /*########################################################################
                           Algorithmns Config
    ########################################################################*/
  val KNN_CONFIG = List(Map("k" -> 0.01), Map("k" -> 0.05), Map("k" -> 0.025), Map("k" -> 0.1))
  val CBLOF_DBSCAN_CONFIG = List(Map("minpoints" -> 10.0, "epsilon" -> 1.5, "alpha" -> 99.0), Map("minpoints" -> 10.0, "epsilon" -> 1.0, "alpha" -> 99.0), Map("minpoints" -> 10.0, "epsilon" -> 0.5, "alpha" -> 99.0), Map("minpoints" -> 6.0, "epsilon" -> 1.5, "alpha" -> 99.0), Map("minpoints" -> 6.0, "epsilon" -> 1.0, "alpha" -> 99.0), Map("minpoints" -> 6.0, "epsilon" -> 0.5, "alpha" -> 99.0))
  val CBLOF_XMEANS_CONFIG = List(Map("kmin" -> 7.0, "alpha" -> 99.0), Map("kmin" -> 5.0, "alpha" -> 99.0), Map("kmin" -> 9.0, "alpha" -> 99.0), Map("kmin" -> 7.0, "alpha" -> 97.0), Map("kmin" -> 5.0, "alpha" -> 97.0), Map("kmin" -> 9.0, "alpha" -> 97.0), Map("kmin" -> 7.0, "alpha" -> 99.5), Map("kmin" -> 5.0, "alpha" -> 99.5), Map("kmin" -> 9.0, "alpha" -> 99.5))
  val LDCOF_XMEANS_CONFIG = List(Map("kmin" -> 7.0, "gamma" -> 0.01), Map("kmin" -> 7.0, "gamma" -> 0.05), Map("kmin" -> 7.0, "gamma" -> 0.1))
  val LDCOF_DBSCAN_CONFIG = List(Map("minpoints" -> 6.0, "epsilon" -> 1.5, "gamma" -> 0.01), Map("minpoints" -> 6.0, "epsilon" -> 1.0, "gamma" -> 0.01), Map("minpoints" -> 6.0, "epsilon" -> 0.5, "gamma" -> 0.01))
  val LOF_CONFIG = List(Map("kmin" -> 0.02, "kmax" -> 0.045), Map("kmin" -> 0.01, "kmax" -> 0.045), Map("kmin" -> 0.03, "kmax" -> 0.06), Map("kmin" -> 0.005, "kmax" -> 0.01))
  val LOOP_CONFIG = List(Map("k" -> 0.05, "norm_factor" -> 0.2), Map("k" -> 0.025, "norm_factor" -> 0.2), Map("k" -> 0.075, "norm_factor" -> 0.2), Map("k" -> 0.05, "norm_factor" -> 0.15), Map("k" -> 0.025, "norm_factor" -> 0.25), Map("k" -> 0.075, "norm_factor" -> 0.225))
  val RLL_ISO_CONFIG = List(Map[String, Double]())
  val RLL_M5P_CONFIG = List(Map[String, Double]())
  val RNN_CONFIG = List(Map[String, Double]())


  val CONFIG_BY_OUTLIER_METHOD: Map[String, List[Map[String, Double]]] = Map("knn" -> KNN_CONFIG, "cblof_regular_db" -> CBLOF_DBSCAN_CONFIG, "cblof_regular_x_means" -> CBLOF_XMEANS_CONFIG, "cblof_x_means" -> CBLOF_XMEANS_CONFIG,
    "ldcof_regular_x_means" -> LDCOF_XMEANS_CONFIG, "ldcof_regular_db_scan" -> LDCOF_DBSCAN_CONFIG, "lcdof_x_means" -> LDCOF_XMEANS_CONFIG, "lof" -> LOF_CONFIG,"lof_regular" -> LOF_CONFIG, "loop" -> LOOP_CONFIG, "rll_m5p" -> RLL_M5P_CONFIG, "rll_iso" -> RLL_ISO_CONFIG, "rnn" -> RNN_CONFIG)

  /*########################################################################
                          Preprocess Configuration
    ########################################################################*/
  val PCA_FIXED_FOLDER = "pca_fixed"
  val PCA_VARIANT_FOLDER = "pca_variant"
  val REMOVE_CORR_FOLDER = "remove_corr"
  val PREPROCESS_KEY = "prepro"

  val PRE_PRO_TECHNIQUES = List("remove_corr", "pca_fixed", "pca_variant")
  val PARAM_CONFIGS_PRE_PRO: Map[String, List[Map[String, Double]]] = Map("pca_variant" -> List(Map("variance" -> 0.85), Map("variance" -> 0.9), Map("variance" -> 0.95), Map("variance" -> 0.97)),
    "pca_fixed" -> List(Map("number" -> 2.0), Map("number" -> 6.0), Map("number" -> 12.0)),
    "remove_corr" -> List(Map("corr_variance" -> 0.85, "min_variance" -> 0.1), Map("corr_variance" -> 0.9, "min_variance" -> 0.1), Map("corr_variance" -> 0.95, "min_variance" -> 0.1))
  )

  /*########################################################################
                         Matching Selection Config
    ########################################################################*/

  val FUZZY_DELTA_SELECTION = List(Map("fuzzy" -> 0.001), Map("fuzzy" -> 0.1), Map("fuzzy" -> 0.01))
  val FUZZY_RATIO_SELECTION = List(Map("fuzzy" -> 1.01), Map("fuzzy" -> 1.02), Map("fuzzy" -> 1.10))
  val GREEDY_SELECTION = List(Map("fuzzy" -> 1.0))
  //val SELECTION_CONFIG = Map("greedy_rank" -> GREEDY_SELECTION, "greedy_rank_fuzzy_delta" -> FUZZY_DELTA_SELECTION, "greedy_rank_fuzzy_ratio" -> FUZZY_RATIO_SELECTION)
  val SELECTION_CONFIG = Map("greedy_rank" -> GREEDY_SELECTION)



  val SELECTION_METHODS_BY_NAME: Map[String, (Double) => (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double]] = Map("greedy_rank"->MatchingSelector.greedyRankSelectorSimpleExp, "greedy_rank_fuzzy_delta" -> MatchingSelector.fuzzyGreedyRankSelectorDelta, "greedy_rank_fuzzy_ratio" -> MatchingSelector.fuzzyGreedyRankSelectorDelta)

  val select_fct = MatchingSelector.fuzzyGreedyRankSelectorDelta(delta_fuzzy_selection)
  val select_fct_Test: (Double) => (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double] = MatchingSelector.fuzzyGreedyRankSelectorDelta


  val mean_computer = new Mean()
  val stdev_computer = new StandardDeviation()
  val ratio_fuzzy_selection = 1.20
  val delta_fuzzy_selection = 0.001


  val matching_pairs: List[(File, File)] = MiscExperiments.getListOfMatchingRefPairs


  /*########################################################################
                        Test Area
  ########################################################################*/


  //runAllNonSeparatedForAllAlgos("thesisexperiments/outliereval", matching_pairs, select_fct )
  //runAllNonSeparated("rnn","thesisexperiments/outliereval", matching_pairs, select_fct)
  //runAllForAllAlgos("thesisexperiments/outliereval/separated", matching_pairs, select_fct,false,"ontos/2014/conference",true )
  //runAllPreproOneMethod("cblof_regular_x_means","thesisexperiments/outliereval/",GREEDY_SELECTION.head, matching_pairs, select_fct,"ontos/2014/conference",true)
  //val processes: OutlierEvaluationProcessesBySepartation = parseOutlierEvaluationProcesses("../RapidminerRepo/OutlierEvaluationV2","oacode_cblof_unweighted_regular_db_scan.rmp")
  //executeProcessSeparated(select_fct,matching_pairs,"ontos/2014/conference",processes.separated.pca_fixed.head,PCA_FIXED_FOLDER,IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS)


  runAllForAllAlgosForAllSltcFunctions("thesisexperiments/outliereval", matching_pairs,false,"ontos/2014/conference" )

  RapidminerJobs.quit()

  def runAllForAllAlgosForAllSltcFunctions(base_folder: String, matching_pairs: List[(File, File)], parallel: Boolean, path_to_ontos_folder: String): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {

    val non_separated_folder = base_folder +"/non_separated"
    createFolder(non_separated_folder)
    val non_separated_best = runAllForAlgosForAllSlctFunctions(non_separated_folder, matching_pairs, parallel, path_to_ontos_folder, false)


    val separated_folder = base_folder +"/separated"
    createFolder(separated_folder)
    val separated_best = runAllForAlgosForAllSlctFunctions(separated_folder, matching_pairs, parallel, path_to_ontos_folder, true)

    val best_result =  if (separated_best._2._2.overall_agg_best.macro_eval_res.f1Measure > non_separated_best._2._2.overall_agg_best.macro_eval_res.f1Measure ) {
      println("separated is best")
      separated_best
    } else {
      println("non separated is best")
      non_separated_best
    }

    HistogramChartFactory.createExecutionSummaryReport(base_folder, "overall_best_result", best_result)

    best_result
  }


  def runAllForAlgosForAllSlctFunctions(base_folder: String, matching_pairs: List[(File, File)], parallel: Boolean, path_to_ontos_folder: String, separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {
    val results = SELECTION_CONFIG.map { case (name, fuzzy_values) => {
      val fuzzy_base_folder = base_folder + "/" + name
      createFolder(fuzzy_base_folder)
      val selct_fct = SELECTION_METHODS_BY_NAME.get(name).get
      runAllForAlgosForOneFuzzySelectFct(name, fuzzy_values, selct_fct, fuzzy_base_folder, matching_pairs, parallel, path_to_ontos_folder, separated)
    }
    }

    val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = results.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure)
    HistogramChartFactory.createExecutionSummaryReport(base_folder, "best_result", best_result)

    best_result
  }


  def runAllForAlgosForOneFuzzySelectFct(method: String, fuzzy_values: List[Map[String, Double]], slct_fct: (Double) => (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], base_folder: String, matching_pairs: List[(File, File)], parallel: Boolean, path_to_ontos_folder: String, separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {


    val results = fuzzy_values.zipWithIndex.map(fuzzy_config => {
      val fuzzy_base_folder = base_folder + "/" + fuzzy_config._2
      createFolder(fuzzy_base_folder)

      val init_slct_fct = slct_fct(fuzzy_config._1.get("fuzzy").get)
      val name_to_config = Map(method -> 1.0)
      val final_config = fuzzy_config._1 ++ name_to_config

      val result = runAllForAllAlgos(fuzzy_base_folder, matching_pairs, final_config, init_slct_fct, parallel, path_to_ontos_folder, separated)


      result
    })


    val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) =   results.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure)
    HistogramChartFactory.createExecutionSummaryReport(base_folder, "best_result", best_result)

    best_result
  }

  /**
   * Runs all non separated outlier methods on the given matching pairs (base_matchings + outlier eval)
   * @param base_folder Base folder to store evaluation results
   * @param matching_pairs matchings pairs
   * @param select_fct Variant of the resulting matching selector (ratio-based fuzzy greedy rank, delta fuzzy greedy rank, greedy rank)
   * @param parallel Execute in parallel (-> only for multicore machines)
   * @return
   */
  def runAllForAllAlgos(base_folder: String, matching_pairs: List[(File, File)], config: Map[String, Double], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], parallel: Boolean, path_to_ontos_folder: String, separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {

    //createFolder(base_folder)
    val results = IMPLEMENTED_OUTLIER_METHODS_BY_NAME.map { case (name, files) => {

      runAllPreproOneMethod(name, base_folder, config, matching_pairs, select_fct, path_to_ontos_folder, separated)
    }
    }

    val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) =  results.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure)
    HistogramChartFactory.createExecutionSummaryReport(base_folder, "best_result", best_result)

    best_result
  }


  /**
   * Runs one non separated outliermethod with different preprocessing and algorith configurations
   * @param outlier_method
   * @param base_folder
   * @param matching_pairs
   * @param select_fct
   * @return
   */
  def runAllPreproOneMethod(outlier_method: String, base_folder: String, select_config: Map[String, Double], matching_pairs: List[(File, File)], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], path_to_ontos_folder: String, separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {
    createFolder(base_folder)
    val processes: OutlierEvaluationProcessesBySepartation = parseOutlierEvaluationProcesses("../RapidminerRepo/OutlierEvaluationV2", IMPLEMENTED_OUTLIER_METHODS_BY_NAME.get(outlier_method).get)

    //check if folder exists
    val base_folder_name: String = base_folder + "/" + outlier_method
    createFolder(base_folder_name)
    val configuration_list = CONFIG_BY_OUTLIER_METHOD.get(outlier_method).get


    println("Size of configs" + configuration_list.size)
    val results = configuration_list.zipWithIndex.par.map{case (config,index) => {
      val base_folder_config = base_folder_name + s"/$index"
      createFolder(base_folder_config)
      //just for reporting purposes
      val total_config = config ++ select_config
      val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = runAlgorithmSingleNonSeparated(index,processes, matching_pairs, select_fct, base_folder_config, total_config, path_to_ontos_folder, separated)

      //print this result to pdf
      HistogramChartFactory.createExecutionSummaryReport(base_folder_config, outlier_method, best_result)
      best_result
    }}


    val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = results.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure)

    HistogramChartFactory.createExecutionSummaryReport(base_folder_name, outlier_method + "_best", best_result)

    best_result


  }

  /**
   * Runs one non separated outlier method for different preprocess methods
   * @param processes
   * @param matching_pairs
   * @param select_fct
   * @param base_folder_name
   * @param config
   * @return
   */
  def runAlgorithmSingleNonSeparated(run_number:Int,processes: OutlierEvaluationProcessesBySepartation, matching_pairs: List[(File, File)], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], base_folder_name: String, config: Map[String, Double], path_to_onto_folder: String, separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {
    val param = Map("mining" -> config)

    val results = PRE_PRO_TECHNIQUES.map(pre_pro_name => {
      //distinguish between different parts


      val result: Option[(String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = if (pre_pro_name.equals(PCA_VARIANT_FOLDER)) {
        val folder_name = base_folder_name + "/" + PCA_VARIANT_FOLDER
        createFolder(folder_name)
        if (!separated) {
          Option((pre_pro_name, runNonSeparated(run_number,folder_name, select_fct, pre_pro_name, processes.non_separated.pca_variance, param, path_to_onto_folder, 9, matching_pairs, separated)))
        } else {
          Option((pre_pro_name, runNonSeparated(run_number,folder_name, select_fct, pre_pro_name, processes.separated.pca_variance, param, path_to_onto_folder, 9, matching_pairs, separated)))
        }
      } else if (pre_pro_name.equals(PCA_FIXED_FOLDER)) {
        val folder_name = base_folder_name + "/" + PCA_FIXED_FOLDER
        createFolder(folder_name)
        if (!separated) {
          Option((pre_pro_name, runNonSeparated(run_number,folder_name, select_fct, pre_pro_name, processes.non_separated.pca_fixed, param, path_to_onto_folder, 9, matching_pairs, separated)))
        } else {
          Option((pre_pro_name, runNonSeparated(run_number,folder_name, select_fct, pre_pro_name, processes.separated.pca_fixed, param, path_to_onto_folder, 9, matching_pairs, separated)))
        }
      } else if (pre_pro_name.equals(REMOVE_CORR_FOLDER)) {
        val folder_name = base_folder_name + "/" + REMOVE_CORR_FOLDER
        createFolder(folder_name)
        if (!separated) {
          Option((pre_pro_name, runNonSeparated(run_number,folder_name, select_fct, pre_pro_name, processes.non_separated.remove_correlated, param, path_to_onto_folder, 9, matching_pairs, separated)))
        } else {
          Option((pre_pro_name, runNonSeparated(run_number,folder_name, select_fct, pre_pro_name, processes.separated.remove_correlated, param, path_to_onto_folder, 9, matching_pairs, separated)))
        }
      } else {
        Option.empty
      }

      result.get
    })

    val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = results.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure)
    val best_result_res = best_result._2

    best_result
  }

  def runNonSeparated(run_number:Int,folder: String, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], name: String, process_files: List[String], parameters: Map[String, Map[String, Double]], path_to_onto_folder: String, top_n: Int, ref_matching_pairs: List[(File, File)], separated: Boolean): (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated) = {

    //Get Parameters
    val pre_pro_param_config: List[Map[String, Double]] = PARAM_CONFIGS_PRE_PRO.get(name).get

    var i = 0;
    val results: List[Option[(Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)]] = pre_pro_param_config.map(config => {
      try {
        val parameter_config: Map[String, Map[String, Double]] = parameters.+(name -> config)

        val results = executeListOfNonSeparatedProcesses(run_number, selection_function, name, process_files, parameter_config, path_to_onto_folder, top_n, ref_matching_pairs, separated)
        println("finshed round " + i)
        HistogramChartFactory.createReportForExecutionRun(folder, s"$name _run_$i", results, parameter_config)
        i = i + 1
        Option((parameter_config, results))
      }
      catch {
        case e:Throwable => {
          e.printStackTrace()
          Option.empty
        }
      }
    })

    val results_filtered = results.filter(_.isDefined).map(_.get)

    val best_result: (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated) = results_filtered.maxBy(_._2.overall_agg_best.macro_eval_res.f1Measure)
    println("Best Result from " + name)


    best_result
  }

  def executeListOfNonSeparatedProcesses(run_number:Int, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], name: String, process_files: List[String], parameters: Map[String, Map[String, Double]], path_to_onto_folder: String, top_n: Int, ref_matching_pairs: List[(File, File)], separated: Boolean): ProcessEvalExecutionResultsNonSeparated = {


    val results: Map[String, ProcessEvalExecutionResultNonSeparated] = process_files.map(process_file => {


      if (separated) {
        process_file -> executeProcessSeparated(run_number,select_fct, ref_matching_pairs, path_to_onto_folder, process_file, name, parameters, IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS)
      } else {
        process_file -> executeProcessNonSeparated(run_number,selection_function, ref_matching_pairs, path_to_onto_folder, process_file, parameters, top_n, name)
      }

    }).toMap

    //get best result by macro f1-measure
    val overall_best_result: (String, ProcessEvalExecutionResultNonSeparated) = results.maxBy(_._2.best_agg_res.macro_eval_res.f1Measure)
    //println(overall_best_result)
    ProcessEvalExecutionResultsNonSeparated(separated, overall_best_result._2.best_agg_res, overall_best_result, results)
  }


  /**
   * Runs one none separated outlier method for one algo config and one prepro config
   * @param selection_function
   * @param ref_matching_pairs
   * @param path_to_onto_folder
   * @param rapidminer_file
   * @param parameters
   * @param top_n
   * @param pre_pro_key
   * @return
   */
  def executeProcessNonSeparated(run_number:Int, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], ref_matching_pairs: List[(File, File)], path_to_onto_folder: String, rapidminer_file: String, parameters: Map[String, Map[String, Double]], top_n: Int, pre_pro_key: String): ProcessEvalExecutionResultNonSeparated = {

    val process_name = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.lastIndexOf("."));
    val process_name_with_ending = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.size);

    println(process_name_with_ending)

    println(IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS)
    val process_type: String = IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS.get(process_name_with_ending).get

    val matching_results_intermediate: List[(Map[String, (Map[MatchRelation, Double], Alignment)], (String, Map[String, Seq[(MatchRelation, Double, Boolean)]]), OutlierEvalStatisticsObject)] = ref_matching_pairs.map { case (ref_file, matching_file) => {
      val name = matching_file.getName.slice(0, matching_file.getName.lastIndexOf("."));

      val result: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) = RapidminerJobs.rapidminerOutlierDetectionExperiments(run_number,rapidminer_file, matching_file, parameters, pre_pro_key, process_type)

      val ref_alignment = AlignmentParser.parseRDF(ref_file)

      val norm_res_gaussian: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGaussianScaling(result._1, result._2, result._3).toMap
      val norm_res_euclidean_max: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByMaxEuclideanDistance(result._1, result._2, result._3).toMap
      val norm_res_gamma: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGammaScaling(result._1, result._2, result._3).toMap
      val norm_res_znorm: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByZScore(result._1, result._2, result._3).toMap
      val resulting_matchings: Map[String, (Map[MatchRelation, Double], Alignment)] = Map(("none", (result._3, ref_alignment)), ("gaussian", (norm_res_gaussian, ref_alignment)), ("zscore", (norm_res_znorm, ref_alignment)), ("gammma", (norm_res_gamma, ref_alignment)), ("euclidean_max", (norm_res_euclidean_max, ref_alignment)))


      val top_n_none: Seq[(MatchRelation, Double, Boolean)] = getTopNResults(result._3, top_n, ref_alignment)
      val top_n_gaussian: Seq[(MatchRelation, Double, Boolean)] = getTopNResults(norm_res_gaussian, top_n, ref_alignment)
      val top_n_euclidean: Seq[(MatchRelation, Double, Boolean)] = getTopNResults(norm_res_euclidean_max, top_n, ref_alignment)
      val top_n_gamma: Seq[(MatchRelation, Double, Boolean)] = getTopNResults(norm_res_gamma, top_n, ref_alignment)
      val top_n_z: Seq[(MatchRelation, Double, Boolean)] = getTopNResults(norm_res_znorm, top_n, ref_alignment)


      val top_n_scores = name -> Map("none" -> top_n_none, "gaussian" -> top_n_gaussian, "euclidean" -> top_n_euclidean, "gamma" -> top_n_gamma, "zscore" -> top_n_z)


      val values = result._3.values.toArray
      val statistics: OutlierEvalStatisticsObject = computeBaseStatisticsOverOutlierScores(values, name)

      (resulting_matchings, top_n_scores, statistics)
    }
    }

    val matching_results: List[Map[String, (Map[MatchRelation, Double], Alignment)]] = matching_results_intermediate.map(_._1)
    val optimization_grid = ParameterOptimizer.getDoubleGrid(0.001, 0.9999999999, 500)


    val threshold_optimized_values: ThresholdOptResult = findOptimalThresholds(selection_function, matching_results, optimization_grid)

    val statistics: List[OutlierEvalStatisticsObject] = matching_results_intermediate.unzip3._3
    val top_n_results: Map[String, Map[String, Seq[(MatchRelation, Double, Boolean)]]] = matching_results_intermediate.unzip3._2.toMap

    //get best normalization technique by max macro f1 measure
    val best_result: (String, (Double, AggregatedEvaluationResult)) = threshold_optimized_values.best_global_results.maxBy(_._2._2.macro_eval_res.f1Measure)

    ProcessEvalExecutionResultNonSeparated(false, best_result._2._2, threshold_optimized_values, statistics, top_n_results, best_result, null, null)
  }

  /*########################################################################
                        Utility Methods
    ########################################################################*/

  def createFolder(base_folder_name: String) {
    val base_folder: File = new File(base_folder_name)
    if (!base_folder.exists()) {
      base_folder.mkdir()
    }
  }


  def getTopNResults(result: Map[MatchRelation, Double], n: Int, ref_alignment: Alignment): Seq[(MatchRelation, Double, Boolean)] = {
    val res_sorted = result.toSeq.sortWith((tuple1, tuple2) => tuple1._2 > tuple2._2)
    val top = res_sorted.zipWithIndex.filter(zipped_tuple => zipped_tuple._2 <= n).unzip._1


    val result_tp: Seq[(MatchRelation, Double, Boolean)] = top.map { case (relation, score) => {

      val cell = new Cell(relation.left, relation.right, score, relation.relation, relation.owl_type)

      if (ref_alignment.correspondences.contains(cell)) {
        (relation, score, true)
      } else {
        (relation, score, false)
      }
    }
    }
    result_tp
  }


  /**
   *
   * Find Local and global optima
   * @param scores_by_norm_technique
   * @param threshold_grid
   * @return
   */
  def findOptimalThresholds(selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], scores_by_norm_technique: List[Map[String, (Map[MatchRelation, Double], Alignment)]], threshold_grid: List[Double]): ThresholdOptResult = {
    val unique_techniques = scores_by_norm_technique.head.keys.toVector

    val results_by_techniques: Map[String, List[(Map[MatchRelation, Double], Alignment)]] = unique_techniques.map(technique => {
      //get results for a techniques
      val matchings_for_technique: List[(Map[MatchRelation, Double], Alignment)] = scores_by_norm_technique.map(elem => elem.get(technique).get)

      (technique, matchings_for_technique)
    }).toMap


    //optimize for each matching technique and find global optimum
    val global_results: Map[String, Seq[(Double, AggregatedEvaluationResult)]] = results_by_techniques.map { case (name, list_of_matchings) => {

      //try for all thresholds
      val results_by_threshold = threshold_grid.map(threshold => {
        val eval_res_single_list: Seq[EvaluationResult] = list_of_matchings.map(single_matchings => {
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

    //get best result
    val best_global_results: Map[String, (Double, AggregatedEvaluationResult)] = global_results.map { case (name, list_of_results) => {
      val best_result: (Double, AggregatedEvaluationResult) = list_of_results.maxBy(_._2.macro_eval_res.f1Measure)
      (name, best_result)
    }
    }

    //find local optima, so for each technique and for each matched dataset the best threshold and result
    val best_local_results: Map[String, Seq[(Double, EvaluationResult)]] = results_by_techniques.map { case (name, list_of_matchings) => {
      name -> list_of_matchings.map(single_matchings => {
        //find best threshold
        val res_by_threshold: Seq[(Double, EvaluationResult)] = threshold_grid.map(threshold => {
          val selected = selection_function(single_matchings._1, threshold)

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
    ThresholdOptResult(global_results, best_global_results, best_local_results, results_local_optima)
  }


  /**
   * Computes basic statistics over the  outlier scores
   * @param values
   * @param name
   */
  def computeBaseStatisticsOverOutlierScores(values: Array[Double], name: String): OutlierEvalStatisticsObject = {

    val stdev = stdev_computer.evaluate(values)
    val mean = mean_computer.evaluate(values)

    val values_sorted = values.sortWith(_ > _).zipWithIndex

    val top_values = values_sorted.filter(tuple => tuple._2 <= 10).toList

    val min_value = values.minBy(value => value)

    val null_hypothesis = new NormalDistribution(mean, stdev)

    val hypo_test = new KolmogorovSmirnovTest()

    val p_value = hypo_test.kolmogorovSmirnovTest(null_hypothesis, values, false) / 2
    val chi_square_test = new ChiSquareTestImpl()

    OutlierEvalStatisticsObject(name, stdev, mean, p_value, values_sorted.head._1, min_value, top_values.unzip._1)
  }


}

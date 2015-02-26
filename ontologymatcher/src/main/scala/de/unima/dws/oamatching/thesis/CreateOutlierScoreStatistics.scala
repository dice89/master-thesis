package de.unima.dws.oamatching.thesis

import java.io.File

import de.unima.dws.oamatching.analysis.{RapidminerJobs, SeparatedResults}
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
case class OutlierEvalStatisticsObject(ds_name: String, stdev: Double, mean: Double, p_value: Double, top_outlier_score: Double, min_outlier_score: Double, top_10_outlier_score: List[Double])

case class TresholdOptResultLocalGlobal(global_results_per_threshold: Map[String, Seq[(Double, AggregatedEvaluationResult)]], best_global_results: Map[String, (Double, AggregatedEvaluationResult)], local_optima_per_ds: Map[String, Seq[(Double, EvaluationResult)]], agg_local_optimum: Map[String, AggregatedEvaluationResult])

case class ProcessEvalExecutionResultNonSeparated(local_global_threshold: TresholdOptResultLocalGlobal, statistics: List[OutlierEvalStatisticsObject], top_n_results: Map[String, Map[String, Seq[(MatchRelation, Double, Boolean)]]], best_result: (String, (Double, AggregatedEvaluationResult)))

case class ProcessEvalExecutionResultsNonSeparated(best_result: (String, ProcessEvalExecutionResultNonSeparated), results: Map[String, ProcessEvalExecutionResultNonSeparated])

object CreateOutlierScoreStatistics extends App with OutlierEvaluationProcessParser {
  RapidminerJobs.init()


  val IMPLEMENTED_OUTLIER_METHODS_BY_NAME = Map("cblof_regular_db" -> "oacode_cblof_unweighted_regular_db_scan.rmp",
    "cblof_regular_x_means" -> "oacode_cblof_unweighted_regular_x_means.rmp",
    "cblof_x_means" -> "oacode_cblof_unweighted_x_means.rmp",
    "knn" -> "oacode_knn.rmp",
    "ldcof_regular_x_means" -> "oacode_ldcof_regular_x_means.rmp",
    "ldcof_regular_db_scan" -> "oacode_ldcof_regular_db_scan.rmp",
    "lcdof_x_means" -> "oacode_ldcof_x_means.rmp",
    "lof_regular" -> "oacode_lof_regular.rmp",
    "lof" -> "oacode_lof.rmp",
    "loop" -> "oacode_loop.rmp")

  val IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS: Map[String, String] = IMPLEMENTED_OUTLIER_METHODS_BY_NAME.map(tuple => (tuple._2, tuple._1))

  val KNN_CONFIG = List(Map("k" -> 0.01), Map("k" -> 0.05), Map("k" -> 0.025), Map("k" -> 0.1))
  val CBLOF_DBSCAN_CONFIG = List(Map("minpoints"->10.0, "epsilon"->1.5,"alpha"->99.0),Map("minpoints"->10.0, "epsilon"->1.0,"alpha"->99.0),Map("minpoints"->10.0, "epsilon"->0.5,"alpha"->99.0),Map("minpoints"->6.0, "epsilon"->1.5,"alpha"->99.0),Map("minpoints"->6.0, "epsilon"->1.0,"alpha"->99.0),Map("minpoints"->6.0, "epsilon"->0.5,"alpha"->99.0))
  val CBLOF_XMEANS_CONFIG = List(Map("kmin"->7.0,"alpha"->99.0),Map("kmin"->5.0,"alpha"->99.0),Map("kmin"->9.0,"alpha"->99.0),Map("kmin"->7.0,"alpha"->97.0),Map("kmin"->5.0,"alpha"->97.0),Map("kmin"->9.0,"alpha"->97.0),Map("kmin"->7.0,"alpha"->99.5),Map("kmin"->5.0,"alpha"->99.5),Map("kmin"->9.0,"alpha"->99.5))
  val LDCOF_XMEANS_CONFIG = List(Map("kmin"->7.0,"gamma"->0.01),Map("kmin"->7.0,"gamma"->0.05),Map("kmin"->7.0,"gamma"->0.1))
  val LDCOF_DBSCAN_CONFIG =  List(Map("minpoints"->6.0, "epsilon"->1.5,"gamma"->0.01),Map("minpoints"->6.0, "epsilon"->1.0,"gamma"->0.01),Map("minpoints"->6.0, "epsilon"->0.5,"gamma"->0.01))
  val LOF_CONFIG = List(Map("kmin"->0.02,"kmax"->0.045),Map("kmin"->0.01,"kmax"->0.045),Map("kmin"->0.03,"kmax"->0.06),Map("kmin"->0.005,"kmax"->0.01))
  val LOOP_CONFIG = List(Map("k"->0.05,"norm_factor"->0.2),Map("k"->0.025,"norm_factor"->0.2),Map("k"->0.075,"norm_factor"->0.2),Map("k"->0.05,"norm_factor"->0.15),Map("k"->0.025,"norm_factor"->0.25),Map("k"->0.075,"norm_factor"->0.225))

  val CONFIG_BY_OUTLIER_METHOD: Map[String, List[Map[String, Double]]] = Map("knn" -> KNN_CONFIG, "cblof_regular_db"->CBLOF_DBSCAN_CONFIG,"cblof_regular_x_means"->CBLOF_XMEANS_CONFIG,"cblof_x_means"->CBLOF_XMEANS_CONFIG,
    "ldcof_regular_x_means"->LDCOF_XMEANS_CONFIG,"ldcof_regular_db_scan"->LDCOF_DBSCAN_CONFIG, "lcdof_x_means"->LDCOF_XMEANS_CONFIG,"lof"->LOF_CONFIG, "loop"->LOOP_CONFIG )


  val PCA_FIXED_FOLDER = "pca_fixed"
  val PCA_VARIANT_FOLDER = "pca_variant"
  val REMOVE_CORR_FOLDER = "remove_corr"

  val PREPROCESS_KEY = "prepro"

  val PRE_PRO_TECHNIQUES = List("remove_corr","pca_fixed", "pca_variant")
  val PARAM_CONFIGS_PRE_PRO: Map[String, List[Map[String, Double]]] = Map("pca_variant" -> List(Map("variance" -> 0.85), Map("variance" -> 0.9), Map("variance" -> 0.95), Map("variance" -> 0.97)),
    "pca_fixed" -> List(Map("number" -> 2.0), Map("number" -> 6.0), Map("number" -> 12.0)),
    "remove_corr" -> List(Map("corr_variance" -> 0.85, "min_variance" -> 0.1), Map("corr_variance" -> 0.9, "min_variance" -> 0.1), Map("corr_variance" -> 0.95, "min_variance" -> 0.1))
  )

  val mean_computer = new Mean()
  val stdev_computer = new StandardDeviation()
  val ratio_fuzzy_selection = 1.20
  val delta_fuzzy_selection = 0.001


  val outlier_processes_for_eval = List("/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluationV2/non_separated/pre_pro_pca_f/oacode_cblof_unweighted_regular_db_scan.rmp")


  // val outlier_processes_for_eval = List("/Users/mueller/Documents/master-thesis/RapidminerRepo/OutlierEvaluation/separated/pre_pro_pca/oacode_cblof_unweighted_regular_db_scan.rmp")


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


  //computeAndVisualizeOutlierScoresSeparated("ontos/2014/conference",outlier_processes_for_eval.head)
  val select_fct = MatchingSelector.fuzzyGreedyRankSelectorDelta(delta_fuzzy_selection)
  val matching_pairs: List[(File, File)] = MiscExperiments.getListOfMatchingRefPairs


  def runAllNonSeparatedForAllAlgos(base_folder:String, matching_pairs: List[(File, File)] , select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double]): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {
    val results = IMPLEMENTED_OUTLIER_METHODS_BY_NAME.map{case(name,files)=> {
      runAllNonSeparated("cblof_regular_x_means","thesisexperiments/outliereval",matching_pairs, select_fct)
    }}

    val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) =  results.maxBy(_._2._2.best_result._2.best_result._2._2.macro_eval_res.f1Measure)
    HistogramChartFactory.createExecutionSummaryReport(base_folder,"best_result",best_result)

    best_result
  }



  def runAllNonSeparated(outlier_method: String, base_folder:String, matching_pairs: List[(File, File)] , select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double]): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {

    val processes: OutlierEvaluationProcessesBySepartation = parseOutlierEvaluationProcesses("../RapidminerRepo/OutlierEvaluationV2", IMPLEMENTED_OUTLIER_METHODS_BY_NAME.get(outlier_method).get)

    //check if folder exists
    val base_folder_name: String =  base_folder+"/"+ outlier_method
    createFolder(base_folder_name)
    val configuration_list = CONFIG_BY_OUTLIER_METHOD.get(outlier_method).get

    var i = 0
    val results = configuration_list.map(config => {
      val base_folder_config = base_folder_name + s"/$i"
      createFolder(base_folder_config)
      val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = runAlgorithmSingleNonSeparated(processes, matching_pairs, select_fct, base_folder_config, config)
      i = i + 1
      //print this result to pdf
      HistogramChartFactory.createExecutionSummaryReport(base_folder_config,outlier_method,best_result)
      best_result
    })

    val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) =  results.maxBy(_._2._2.best_result._2.best_result._2._2.macro_eval_res.f1Measure)

    HistogramChartFactory.createExecutionSummaryReport(base_folder_name,outlier_method+"_best",best_result)

    best_result
  }

  def runAlgorithmSingleNonSeparated(processes: OutlierEvaluationProcessesBySepartation, matching_pairs: List[(File, File)], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], base_folder_name: String, config: Map[String, Double]): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {
    val param = Map("mining" -> config)

    val results = PRE_PRO_TECHNIQUES.map(pre_pro_name => {
      //distinguish between different parts
      val result: Option[(String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = if (pre_pro_name.equals(PCA_VARIANT_FOLDER)) {
        val folder_name = base_folder_name + "/" + PCA_VARIANT_FOLDER
        createFolder(folder_name)
        Option((pre_pro_name, runNonSeparated(folder_name, select_fct, pre_pro_name, processes.non_separated.pca_variance, param, "", 9, matching_pairs)))
      } else if (pre_pro_name.equals(PCA_FIXED_FOLDER)) {
        val folder_name = base_folder_name + "/" + PCA_FIXED_FOLDER
        createFolder(folder_name)
        Option((pre_pro_name, runNonSeparated(folder_name, select_fct, pre_pro_name, processes.non_separated.pca_fixed, param, "", 9, matching_pairs)))
      } else if (pre_pro_name.equals(REMOVE_CORR_FOLDER)) {
        val folder_name = base_folder_name + "/" + REMOVE_CORR_FOLDER
        createFolder(folder_name)
        Option((pre_pro_name, runNonSeparated(folder_name, select_fct, pre_pro_name, processes.non_separated.remove_correlated, param, "", 9, matching_pairs)))

      } else {
        Option.empty
      }
      result.get
    })

    val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = results.maxBy(_._2._2.best_result._2.best_result._2._2.macro_eval_res.f1Measure)
    val best_result_res = best_result._2

    println("Pre Pro Technique:" + best_result_res._1)
    println("Process: " + best_result_res._2.best_result._1)
    println("Norm Technique: " + best_result_res._2.best_result._2.best_result._1)
    println("Threshold: " + best_result_res._2.best_result._2.best_result._2._1)
    println("F-Measure (Macro): " + best_result_res._2.best_result._2.best_result._2._2.macro_eval_res)
    println("F-Measure (Micro): " + best_result_res._2.best_result._2.best_result._2._2.micro_eval_res)
    println("Param config:" + best_result._1)


    best_result
  }

  def executeProcessNonSeparated(selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], ref_matching_pairs: List[(File, File)], path_to_onto_folder: String, rapidminer_file: String, parameters: Map[String, Map[String, Double]], top_n: Int, pre_pro_key: String): ProcessEvalExecutionResultNonSeparated = {

    val process_name = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.lastIndexOf("."));
    val process_name_with_ending = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.size);

    println(process_name_with_ending)

    println(IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS)
    val process_type: String = IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS.get(process_name_with_ending).get

    val matching_results_intermediate: List[(Map[String, (Map[MatchRelation, Double], Alignment)], (String, Map[String, Seq[(MatchRelation, Double, Boolean)]]), OutlierEvalStatisticsObject)] = ref_matching_pairs.map { case (ref_file, matching_file) => {
      val name = matching_file.getName.slice(0, matching_file.getName.lastIndexOf("."));

      val result: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) = RapidminerJobs.rapidminerOutlierDetectionExperiments(rapidminer_file, matching_file, parameters, pre_pro_key, process_type)

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


    val threshold_optimized_values: TresholdOptResultLocalGlobal = findOptimalThresholds(selection_function, matching_results, optimization_grid)

    val statistics: List[OutlierEvalStatisticsObject] = matching_results_intermediate.unzip3._3
    val top_n_results: Map[String, Map[String, Seq[(MatchRelation, Double, Boolean)]]] = matching_results_intermediate.unzip3._2.toMap

    //get best normalization technique by max macro f1 measure
    val best_result: (String, (Double, AggregatedEvaluationResult)) = threshold_optimized_values.best_global_results.maxBy(_._2._2.macro_eval_res.f1Measure)

    ProcessEvalExecutionResultNonSeparated(threshold_optimized_values, statistics, top_n_results, best_result)
  }


  def executeListOfNonSeparatedProcesses(selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], name: String, process_files: List[String], parameters: Map[String, Map[String, Double]], path_to_onto_folder: String, top_n: Int, ref_matching_pairs: List[(File, File)]): ProcessEvalExecutionResultsNonSeparated = {

    val results: Map[String, ProcessEvalExecutionResultNonSeparated] = process_files.map(process_file => {
      process_file -> executeProcessNonSeparated(selection_function, ref_matching_pairs, path_to_onto_folder, process_file, parameters, top_n, name)
    }).toMap

    //get best result by macro f1-measure
    val overall_best_result: (String, ProcessEvalExecutionResultNonSeparated) = results.maxBy(_._2.best_result._2._2.macro_eval_res.f1Measure)
    //println(overall_best_result)
    ProcessEvalExecutionResultsNonSeparated(overall_best_result, results)
  }


  def createFolder(base_folder_name: String) {
    val base_folder: File = new File(base_folder_name)
    if (!base_folder.exists()) {
      base_folder.mkdir()
    }
  }

  def runNonSeparated(folder: String, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], name: String, process_files: List[String], parameters: Map[String, Map[String, Double]], path_to_onto_folder: String, top_n: Int, ref_matching_pairs: List[(File, File)]): (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated) = {

    //Get Parameters
    val pre_pro_param_config: List[Map[String, Double]] = PARAM_CONFIGS_PRE_PRO.get(name).get

    var i = 0;
    val results: List[(Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)] = pre_pro_param_config.map(config => {
      val parameter_config: Map[String, Map[String, Double]] = parameters.+(name -> config)

      val results = executeListOfNonSeparatedProcesses(selection_function, name, process_files, parameter_config, "not needed", top_n, ref_matching_pairs)
      println("finshed round " + i)
      HistogramChartFactory.createReportForExecutionRun(folder, s"$name _run_$i", results, parameter_config)
      i = i + 1
      (parameter_config, results)
    })

    val best_result: (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated) = results.maxBy(_._2.best_result._2.best_result._2._2.macro_eval_res.f1Measure)
    println("Best Result from " + name)


    best_result
  }


  /*

    /**
     * Calls the outlier detections script and plotts the outlier distribution in a histogram
     * @param rapidminer_file
     */
    def computeAndVisualizeOutlierScores(rapidminer_file: String): Unit = {
      val pairs: List[(File, File)] = MiscExperiments.getListOfMatchingRefPairs

      val technique_name = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.lastIndexOf("."));


      val chart_data: List[(Seq[(JFreeChart, JFreeChart)], OutlierEvalStatisticsObject, Seq[Seq[(MatchRelation, Double)]], Alignment, Alignment, Map[String, (Map[MatchRelation, Double], Alignment)])] = pairs.map { case (ref_file, matching_file) => {

        val result: (Int, Map[MatchRelation, Double]) = RapidminerJobs.rapidminerOutlierDetectionExperiments(rapidminer_file, matching_file)

        val ref_alignment = AlignmentParser.parseRDF(ref_file)
        //compute statistics
        val values = result._2.values.toArray

        //normalizes values
        val norm_res_gaussian: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGaussianScaling(result._1, result._2).toMap
        val norm_res_euclidean_max: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByMaxEuclideanDistance(result._1, result._2).toMap
        val norm_res_gamma: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGammaScaling(result._1, result._2).toMap
        val norm_res_znorm: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByZScore(result._1, result._2).toMap

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
      HistogramChartFactory.storeMultipleChartsIntoPDF(chart_data.toSeq, s"thesisexperiments/outliereval/$technique_name.pdf", table_data, precision_recall_curve_data)
    }*/

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


  def findOptimalThresholdGlobalOnly(scores_by_norm_technique: List[Map[String, (Map[MatchRelation, Double], Alignment)]], threshold_grid: List[Double]): Map[String, (Double, AggregatedEvaluationResult)] = {
    val unique_techniques = scores_by_norm_technique.head.keys.toVector

    val results_by_techniques: Map[String, List[(Map[MatchRelation, Double], Alignment)]] = unique_techniques.map(technique => {
      //get results for a techniques
      val matchings_for_technique: List[(Map[MatchRelation, Double], Alignment)] = scores_by_norm_technique.map(elem => elem.get(technique).get)

      (technique, matchings_for_technique)
    }).toMap

    println("results by size")
    results_by_techniques.foreach(elem => println(elem._2.size))

    //optimize for each matching technique and find global optimum
    val global_results: Map[String, Seq[(Double, AggregatedEvaluationResult)]] = results_by_techniques.map { case (name, list_of_matchings) => {

      //try for all thresholds
      val results_by_threshold = threshold_grid.map(threshold => {
        val eval_res_single_list: Seq[EvaluationResult] = list_of_matchings.map(single_matchings => {
          // val selected = MatchingSelector.fuzzyGreedyRankSelectorDelta(single_matchings._1, threshold, delta_fuzzy_selection)
          val selected = MatchingSelector.greedyRankSelectorSimple(single_matchings._1, threshold)

          val alignment = new Alignment(null, null, selected)

          alignment.evaluate(single_matchings._2)
        })
        val agg_res = EvaluationMatchingRunner.computeAggregatedResults(eval_res_single_list.toList)
        (threshold, agg_res)
      })
      (name, results_by_threshold)
    }
    }.toMap

    val best_global_results: Map[String, (Double, AggregatedEvaluationResult)] = global_results.map { case (name, list_of_results) => {
      val best_result: (Double, AggregatedEvaluationResult) = list_of_results.maxBy(_._2.macro_eval_res.precision)


      //handle edge case best global result in terms of f-measure is 0.0 -> then take that result that minimized the fp
      if (best_result._2.macro_eval_res.f1Measure == 0.0) {
        val edge_case_best_result = list_of_results.minBy(_._2.micro_eval_res.falsePositives)
        (name, edge_case_best_result)
      } else {
        (name, best_result)
      }
    }
    }

    best_global_results
  }


  /**
   *
   * Find Local and global optima
   * @param scores_by_norm_technique
   * @param threshold_grid
   * @return
   */
  def findOptimalThresholds(selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], scores_by_norm_technique: List[Map[String, (Map[MatchRelation, Double], Alignment)]], threshold_grid: List[Double]): TresholdOptResultLocalGlobal = {
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
    TresholdOptResultLocalGlobal(global_results, best_global_results, best_local_results, results_local_optima)
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

  def computeAndVisualizeOutlierScoresSeparated(path_to_onto_folder: String, rapidminer_file: String): Unit = {
    val pairs: List[(File, File)] = MiscExperiments.getListOfMatchingRefPairs

    val technique_name = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.lastIndexOf("."));


    val normalized_per_category = pairs.map { case (ref_file, matching_file) => {

      //get ontos
      val ontos: List[String] = ref_file.getName().split("-").toList
      val name_onto1: String = path_to_onto_folder + File.separator + ontos(0).replaceAll("-", "") + ".owl"
      val name_onto2: String = path_to_onto_folder + File.separator + ontos(1).replaceAll("-", "").replaceAll(".rdf", "") + ".owl"

      val ref_alignment = AlignmentParser.parseRDFWithOntos(ref_file.getAbsolutePath(), name_onto1, name_onto2)

      println(ref_file)

      //build different reference alignments out of original one for different classes
      val classes_alignment = new Alignment(null, null, ref_alignment.correspondences.filter(_.owl_type.equals(Cell.TYPE_CLASS)).toList)
      val op_alignment = new Alignment(null, null, ref_alignment.correspondences.filter(_.owl_type.equals(Cell.TYPE_OBJECT_PROPERTY)).toList)
      val dp_alignment = new Alignment(null, null, ref_alignment.correspondences.filter(_.owl_type.equals(Cell.TYPE_DT_PROPERTY)).toList)

      val result: SeparatedResults = RapidminerJobs.rapidminerOutlierDetectionExperimentsSeparated(rapidminer_file, matching_file, null)

      val class_normalized = getNormalizedScores(classes_alignment, result.class_matchings)
      val dp_normalized = getNormalizedScores(dp_alignment, result.dp_matchings)
      val op_normalized = getNormalizedScores(op_alignment, result.op_matchings)

      ((class_normalized, dp_normalized, op_normalized), (ref_alignment))
    }
    }.toList

    val tuple_wise: (List[Map[String, (Map[MatchRelation, Double], Alignment)]], List[Map[String, (Map[MatchRelation, Double], Alignment)]], List[Map[String, (Map[MatchRelation, Double], Alignment)]]) = normalized_per_category.unzip._1.unzip3


    val optimization_grid = ParameterOptimizer.getDoubleGrid(0.001, 0.9999999999, 1000)
    //globally normalize each tuple
    val best_threshold_classes: Map[String, (Double, AggregatedEvaluationResult)] = findOptimalThresholdGlobalOnly(tuple_wise._1, optimization_grid)
    val best_threshold_dps: Map[String, (Double, AggregatedEvaluationResult)] = findOptimalThresholdGlobalOnly(tuple_wise._2, optimization_grid)
    val best_threshold_ops: Map[String, (Double, AggregatedEvaluationResult)] = findOptimalThresholdGlobalOnly(tuple_wise._3, optimization_grid)

    //construct final matchings and evaluate
    val best_results = best_threshold_classes.keys.map(norm_technique => {
      val class_threshold = best_threshold_classes.get(norm_technique).get._1
      val dp_threshold = best_threshold_dps.get(norm_technique).get._1
      val op_threshold = best_threshold_dps.get(norm_technique).get._1

      val results = normalized_per_category.map { case ((classes_matchings, dp_matchings, op_matchings), ref_align) => {

        val selected_classes: Map[MatchRelation, Double] = MatchingSelector.greedyRankSelectorSimple(classes_matchings.get(norm_technique).get._1, class_threshold)
        val selected_dps: Map[MatchRelation, Double] = MatchingSelector.greedyRankSelectorSimple(dp_matchings.get(norm_technique).get._1, dp_threshold)
        val selected_ops: Map[MatchRelation, Double] = MatchingSelector.greedyRankSelectorSimple(op_matchings.get(norm_technique).get._1, op_threshold)

        val all_matchings_list = selected_classes ++ selected_dps ++ selected_ops

        val alignment = new Alignment(null, null, all_matchings_list)

        alignment.evaluate(ref_align)
      }
      }.toList

      val agg_res = EvaluationMatchingRunner.computeAggregatedResults(results)
      println(agg_res)
      norm_technique ->(class_threshold, dp_threshold, op_threshold, agg_res)
    })

    best_results.foreach(norm_map => println(norm_map._1 + " " + norm_map._2._4.macro_eval_res.f1Measure))


  }


  def getNormalizedScores(ref_alignment: Alignment, test: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double])): Map[String, (Map[MatchRelation, Double], Alignment)] = {
    val norm_res_gaussian: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGaussianScaling(test._1, test._2, test._3).toMap
    val norm_res_euclidean_max: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByMaxEuclideanDistance(test._1, test._2, test._3).toMap
    val norm_res_gamma: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGammaScaling(test._1, test._2, test._3).toMap
    val norm_res_znorm: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByZScore(test._1, test._2, test._3).toMap

    val resulting_matchings: Map[String, (Map[MatchRelation, Double], Alignment)] = Map(("none", (test._3, ref_alignment)), ("gaussian", (norm_res_gaussian, ref_alignment)), ("zscore", (norm_res_znorm, ref_alignment)), ("gammma", (norm_res_gamma, ref_alignment)), ("euclidean_max", (norm_res_euclidean_max, ref_alignment)))

    resulting_matchings
  }


}

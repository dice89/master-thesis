package de.unima.dws.oamatching.thesis

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationDataSetParser, EvaluationMatchingTask}
import de.unima.dws.oamatching.pipeline.{MatchingPipelineCore, MatchingSelector}
import org.apache.commons.math.stat.descriptive.moment.Mean
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation

import scala.collection.JavaConversions._
import scala.collection.immutable.Map


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


/**
 * Runnable Object that performs a paramter optimization on a problem specified in the config for configured algorithms and precprocessing methods
 * @author Alexander C. Mueller
 */
object CreateOutlierScoreStatistics extends App with OutlierEvaluationProcessParser with SeparatedOptimization with EvaluationDataSetParser with NonSeparatedOptimization with LazyLogging {
  RapidminerJobs.init()

  if(Config.loaded_config.getBoolean("optimization.init_base_matcher")){
    MatcherRegistry.initLargeScale()
  }
  /*########################################################################
                         Algorithms
    ########################################################################*/
  val IMPLEMENTED_OUTLIER_METHODS_BY_NAME = Map(
    "knn" -> "oacode_knn.rmp",
    "rll_m5p" -> "oacode_rll_m5p.rmp",
    "rll_iso" -> "oacode_rll_iso.rmp",
    "cblof_regular_db" -> "oacode_cblof_unweighted_regular_db_scan.rmp",
    "ldcof_regular_x_means" -> "oacode_ldcof_regular_x_means.rmp",
    "ldcof_regular_db_scan" -> "oacode_ldcof_regular_db_scan.rmp",
    "lcdof_x_means" -> "oacode_ldcof_x_means.rmp",
    "lof_regular" -> "oacode_lof_regular.rmp",
    "lof" -> "oacode_lof.rmp",
    "loop" -> "oacode_loop.rmp",
    "cblof_regular_x_means" -> "oacode_cblof_unweighted_regular_x_means.rmp",
    "cblof_x_means" -> "oacode_cblof_unweighted_x_means.rmp",
    "rnn" -> "oacode_rnn.rmp"
  )

  val IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS: Map[String, String] = IMPLEMENTED_OUTLIER_METHODS_BY_NAME.map(tuple => (tuple._2, tuple._1))

  /*########################################################################
                           Algorithmns Config
    ########################################################################*/
  val KNN_CONFIG = List(Map("k" -> 0.025), Map("k" -> 0.05), Map("k" -> 0.01), Map("k" -> 0.1))
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
    "ldcof_regular_x_means" -> LDCOF_XMEANS_CONFIG, "ldcof_regular_db_scan" -> LDCOF_DBSCAN_CONFIG, "lcdof_x_means" -> LDCOF_XMEANS_CONFIG, "lof" -> LOF_CONFIG, "lof_regular" -> LOF_CONFIG, "loop" -> LOOP_CONFIG, "rll_m5p" -> RLL_M5P_CONFIG, "rll_iso" -> RLL_ISO_CONFIG, "rnn" -> RNN_CONFIG)

  /*########################################################################
                          Preprocess Configuration
    ########################################################################*/
  val PCA_FIXED_FOLDER = "pca_fixed"
  val PCA_VARIANT_FOLDER = "pca_variant"
  val REMOVE_CORR_FOLDER = "remove_corr"
  val PREPROCESS_KEY = "prepro"

  val PRE_PRO_TECHNIQUES = List("remove_corr")
  //val PRE_PRO_TECHNIQUES = List("remove_corr", "pca_fixed", "pca_variant")
  /*val PARAM_CONFIGS_PRE_PRO: Map[String, List[Map[String, Double]]] = Map("pca_variant" -> List(Map("variance" -> 0.85), Map("variance" -> 0.9), Map("variance" -> 0.95), Map("variance" -> 0.97)),
    "pca_fixed" -> List(Map("number" -> 2.0), Map("number" -> 4.0),Map("number" -> 6.0), Map("number" -> 12.0)),
    "remove_corr" -> List(Map("corr_variance" -> 0.75, "min_variance" -> 0.01), Map("corr_variance" -> 0.5, "min_variance" -> 0.01), Map("corr_variance" -> 0.9, "min_variance" -> 0.01))
  )*/
  val PARAM_CONFIGS_PRE_PRO: Map[String, List[Map[String, Double]]] = Map("pca_variant" -> List(Map("variance" -> 0.85), Map("variance" -> 0.9), Map("variance" -> 0.95), Map("variance" -> 0.97)),
    "pca_fixed" -> List(Map("number" -> 2.0), Map("number" -> 4.0),Map("number" -> 6.0), Map("number" -> 12.0)),
    "remove_corr" -> List(Map("corr_variance" -> 0.75, "min_variance" -> 0.01))
  )

  /*########################################################################
                         Matching Selection Config
    ########################################################################*/

  val FUZZY_DELTA_SELECTION = List(Map("fuzzy" -> 0.02), Map("fuzzy" -> 0.01), Map("fuzzy" -> 0.001))
  val FUZZY_RATIO_SELECTION = List(Map("fuzzy" -> 1.01), Map("fuzzy" -> 1.02), Map("fuzzy" -> 1.10))
  val GREEDY_SELECTION = List(Map("fuzzy" -> 1.0))

  val SELECTION_CONFIG = Map("greedy_rank_fuzzy_delta" -> FUZZY_DELTA_SELECTION, "greedy_rank_fuzzy_ratio" -> FUZZY_RATIO_SELECTION, "greedy_rank" -> GREEDY_SELECTION)
  //val SELECTION_CONFIG = Map("greedy_rank" -> GREEDY_SELECTION)
  val SELECTION_METHODS_BY_NAME: Map[String, (Double) => (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double]] = Map("greedy_rank" -> MatchingSelector.greedyRankSelectorSimpleExp, "greedy_rank_fuzzy_delta" -> MatchingSelector.fuzzyGreedyRankSelectorDelta, "greedy_rank_fuzzy_ratio" -> MatchingSelector.fuzzyGreedyRankSelectorDelta)

  /*########################################################################
                       Data Set Config
   ########################################################################*/

  val DS_LOCATION_BY_NAME = Map("benchmarks" -> "ontos/2014/benchmarks", "conference" -> "ontos/2014/conference", "anatomy" -> "ontos/2014/anatomy")


  /*########################################################################
                    Init stuff
 ########################################################################*/

  val mean_computer = new Mean()
  val stdev_computer = new StandardDeviation()


  /*########################################################################
                     Build run configurations ---> e.g. choose which selection method to use and so on
 ########################################################################*/

  val configured_selection_strategies = createSelectionBasedOnConfig()

  val configured_algorithms_to_run = createOutlierAlgoBasedOnConfig()

  val dataset = Config.loaded_config.getString("optimization.dataset")
  val base_result_location = Config.loaded_config.getString("optimization.base_folder")

  logger.info("Start Optimization with Scala Version " + util.Properties.versionNumberString)

  logger.info(s" Selection Strategies: $configured_selection_strategies")
  logger.info(s" Outlier Algorithms: $configured_algorithms_to_run")
  logger.info(s" Dataset: $dataset")
  logger.info(s" Result path: $base_result_location")

  startOptimizationForGivenDataset(base_result_location, dataset)

  RapidminerJobs.quit()

  def startOptimizationForGivenDataset(base_folder: String, ds_name: String): Unit = {

    val ds_location = DS_LOCATION_BY_NAME.get(ds_name).getOrElse("ontos/2014/conference")
    logger.info("Location of Ontologies to match " + ds_location)

    val problems = parseProblems(ds_name, ds_location)
    val base_matchings_folder = "matchings" + File.separator + ds_name
    val base_folder_file = new File(base_matchings_folder)

    if (!base_folder_file.exists()) {

      base_folder_file.mkdir()
      createFolder(base_matchings_folder + "/matchings")
      problems.foreach(task => {
        val feature_vector = MatchingPipelineCore.createFeatureVector(task.matching_problem, 0.5, true)


        logger.info("Created Feature Vector of Size " + feature_vector.transposed_vector.size)

        RapidminerJobs.writeCSV(task.matching_problem.name, base_matchings_folder)(feature_vector)
      })
    }

    val eval_folder_name = base_folder + File.separator + ds_name
    createFolder(eval_folder_name)
    val data_set = parseProblems(ds_name, "ontos/2014/" + ds_name)
    val matching_pairs: List[(EvaluationMatchingTask, File)] = getListofProblemMatchingTasks(data_set, base_matchings_folder + File.separator + "matchings")

    runAllForAllAlgosForAllSltcFunctions(ds_name, eval_folder_name, matching_pairs, true)
  }


  /**
   * Functon that facilitates the whole run for both separated and non separated results
   * @param ds_name
   * @param base_folder
   * @param matching_pairs
   * @param parallel
   * @return
   */
  def runAllForAllAlgosForAllSltcFunctions(ds_name: String, base_folder: String, matching_pairs: List[(EvaluationMatchingTask, File)], parallel: Boolean): Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = {

    createFolder(base_folder)


    val non_separated_folder = base_folder + "/non_separated"
    createFolder(non_separated_folder)
    val non_separated_best: Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = runAllForAlgosForAllSlctFunctions(ds_name, non_separated_folder, matching_pairs, parallel, false)

    val separated_folder = base_folder + "/separated"
    createFolder(separated_folder)
    val separated_best: Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = runAllForAlgosForAllSlctFunctions(ds_name, separated_folder, matching_pairs, parallel, true)

    val best_result: Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = if (separated_best.isDefined && non_separated_best.isDefined) {
      val result = if (separated_best.get._2._2.overall_agg_best.macro_eval_res.f1Measure > non_separated_best.get._2._2.overall_agg_best.macro_eval_res.f1Measure) {

        logger.info("Separated method is the best")
        separated_best
      } else {
        logger.info("Non separated method is the best")
        non_separated_best
      }
      result
    } else {
      Option.empty
    }


    if (best_result.isDefined) {
      SummaryPDFFactory.createExecutionSummaryReport(base_folder, "overall_best_result", best_result.get)
    }

    best_result
  }


  /**
   * Runs everything for all configured algorithms and can be called for both separated and not separated matching types
   * @param ds_name
   * @param base_folder
   * @param matching_pairs
   * @param parallel
   * @param separated
   * @return
   */
  def runAllForAlgosForAllSlctFunctions(ds_name: String, base_folder: String, matching_pairs: List[(EvaluationMatchingTask, File)], parallel: Boolean, separated: Boolean): Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = {
    val results = configured_selection_strategies.map { case (name, fuzzy_values) => {
      logger.info(s"Start Outlier Analysis for selection strategy $name")
      val fuzzy_base_folder = base_folder + "/" + name
      createFolder(fuzzy_base_folder)
      val selct_fct = SELECTION_METHODS_BY_NAME.get(name).get
      runAllForAlgosForOneFuzzySelectFct(ds_name, name, fuzzy_values, selct_fct, fuzzy_base_folder, matching_pairs, parallel, separated)
    }
    }

    val results_filtered = results.filter(_.isDefined).map(_.get)

    val best_result: Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = if (results_filtered.size > 0) {
      Option(results_filtered.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure))
    } else {
      Option.empty
    }

    if (best_result.isDefined) {
      SummaryPDFFactory.createExecutionSummaryReport(base_folder, "best_result", best_result.get)

    }

    best_result
  }

  /**
   * Function that runs all algorithms for all selections functions
   * @param ds_name
   * @param method
   * @param fuzzy_values
   * @param slct_fct
   * @param base_folder
   * @param matching_pairs
   * @param parallel
   * @param separated
   * @return
   */
  def runAllForAlgosForOneFuzzySelectFct(ds_name: String, method: String, fuzzy_values: List[Map[String, Double]], slct_fct: (Double) => (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], base_folder: String, matching_pairs: List[(EvaluationMatchingTask, File)], parallel: Boolean, separated: Boolean): Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = {

    val results = fuzzy_values.zipWithIndex.map(fuzzy_config => {
      val fuzzy_base_folder = base_folder + "/" + fuzzy_config._2
      createFolder(fuzzy_base_folder)

      val init_slct_fct = slct_fct(fuzzy_config._1.get("fuzzy").get)
      val name_to_config = Map(method -> 1.0)
      val final_config = fuzzy_config._1 ++ name_to_config

      val result = runAllForAllAlgos(ds_name, fuzzy_base_folder, matching_pairs, final_config, init_slct_fct, parallel, separated)

      result
    })
    val results_filtered = results.filter(_.isDefined).map(_.get)

    val best_result: Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = if (results_filtered.size > 0) {
      Option(results_filtered.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure))
    } else {
      Option.empty
    }

    if (best_result.isDefined) {
      SummaryPDFFactory.createExecutionSummaryReport(base_folder, "best_result", best_result.get)

    }

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
  def runAllForAllAlgos(ds_name: String, base_folder: String, matching_pairs: List[(EvaluationMatchingTask, File)], config: Map[String, Double], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], parallel: Boolean, separated: Boolean): Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = {

    //createFolder(base_folder)
    val results = configured_algorithms_to_run.map { case (name, files) => {

      runAllPreproOneMethod(ds_name, name, base_folder, config, matching_pairs, select_fct, separated)
    }
    }
    val results_filtered = results.filter(_.isDefined).map(_.get)
    //val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) =  results.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure)
    //SummaryPDFFactory.createExecutionSummaryReport(base_folder, "best_result", best_result)

    val best_result: Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = if (results_filtered.size > 0) {
      Option(results_filtered.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure))
    } else {
      Option.empty
    }

    if (best_result.isDefined) {
      SummaryPDFFactory.createExecutionSummaryReport(base_folder, "best_result", best_result.get)
    }

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
  def runAllPreproOneMethod(ds_name: String, outlier_method: String, base_folder: String, select_config: Map[String, Double], matching_pairs: List[(EvaluationMatchingTask, File)], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], separated: Boolean): Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = {
    createFolder(base_folder)
    val processes: OutlierEvaluationProcessesBySepartation = parseOutlierEvaluationProcesses("../RapidminerRepo/OutlierEvaluationV2", IMPLEMENTED_OUTLIER_METHODS_BY_NAME.get(outlier_method).get)

    //check if folder exists
    val base_folder_name: String = base_folder + "/" + outlier_method
    createFolder(base_folder_name)
    val configuration_list = CONFIG_BY_OUTLIER_METHOD.get(outlier_method).get

    logger.info(s"Start Outlier Analysis with $outlier_method and "+  configuration_list.size + " configurations")

    val results: List[Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))]] = configuration_list.zipWithIndex.map { case (config, index) => {
      val base_folder_config = base_folder_name + s"/$index"
      createFolder(base_folder_config)
      //just for reporting purposes
      val total_config = config ++ select_config
      val best_result: Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = runAlgorithmSingleNonSeparated(ds_name, index, processes, matching_pairs, select_fct, base_folder_config, total_config, separated)

      if (best_result.isDefined) {
        SummaryPDFFactory.createExecutionSummaryReport(base_folder_config, outlier_method, best_result.get)
      }
      best_result
    }
    }


    val results_filtered = results.filter(_.isDefined).map(_.get)

    val best_result: Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = if (results_filtered.size > 0) {
      Option(results_filtered.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure))
    } else {
      Option.empty
    }

    //val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = results_filtered.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure)

    if (best_result.isDefined) {
      SummaryPDFFactory.createExecutionSummaryReport(base_folder_name, outlier_method + "_best", best_result.get)
    }

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
  def runAlgorithmSingleNonSeparated(ds_name: String, run_number: Int, processes: OutlierEvaluationProcessesBySepartation, matching_pairs: List[(EvaluationMatchingTask, File)], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], base_folder_name: String, config: Map[String, Double], separated: Boolean): Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = {
    val param = Map("mining" -> config)

    val results = PRE_PRO_TECHNIQUES.map(pre_pro_name => {
      //distinguish between different parts


      logger.info(s"Start Outlier Analysis with pre pro technique $pre_pro_name ")

      val result: Option[(String, Option[(Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)])] = if (pre_pro_name.equals(PCA_VARIANT_FOLDER)) {
        val folder_name = base_folder_name + "/" + PCA_VARIANT_FOLDER
        createFolder(folder_name)
        if (!separated) {
          Option((pre_pro_name, runNonSeparated(ds_name, run_number, folder_name, select_fct, pre_pro_name, processes.non_separated.pca_variance, param, 9, matching_pairs, separated)))
        } else {
          Option((pre_pro_name, runNonSeparated(ds_name, run_number, folder_name, select_fct, pre_pro_name, processes.separated.pca_variance, param, 9, matching_pairs, separated)))
        }
      } else if (pre_pro_name.equals(PCA_FIXED_FOLDER)) {
        val folder_name = base_folder_name + "/" + PCA_FIXED_FOLDER
        createFolder(folder_name)
        if (!separated) {
          Option((pre_pro_name, runNonSeparated(ds_name, run_number, folder_name, select_fct, pre_pro_name, processes.non_separated.pca_fixed, param, 9, matching_pairs, separated)))
        } else {
          Option((pre_pro_name, runNonSeparated(ds_name, run_number, folder_name, select_fct, pre_pro_name, processes.separated.pca_fixed, param, 9, matching_pairs, separated)))
        }
      } else if (pre_pro_name.equals(REMOVE_CORR_FOLDER)) {


        val folder_name = base_folder_name + "/" + REMOVE_CORR_FOLDER
        createFolder(folder_name)
        if (!separated) {
          Option((pre_pro_name, runNonSeparated(ds_name, run_number, folder_name, select_fct, pre_pro_name, processes.non_separated.remove_correlated, param, 9, matching_pairs, separated)))
        } else {
          Option((pre_pro_name, runNonSeparated(ds_name, run_number, folder_name, select_fct, pre_pro_name, processes.separated.remove_correlated, param, 9, matching_pairs, separated)))
        }
      } else {
        Option.empty
      }

      result.get
    })


    val results_filtered = results.filter(_._2.isDefined).map(tuple => tuple._1 -> tuple._2.get)


    val best_result: Option[(String, (Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = if (results_filtered.size > 0) {
      Option(results_filtered.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure))
    } else {
      logger.error("Empty result! Check outlier processes")
      Option.empty
    }



    best_result
  }

  /**
   * TODO
   * @param ds_name
   * @param run_number
   * @param folder
   * @param selection_function
   * @param name
   * @param process_files
   * @param parameters
   * @param top_n
   * @param ref_matching_pairs
   * @param separated
   * @return
   */
  def runNonSeparated(ds_name: String, run_number: Int, folder: String, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], name: String, process_files: List[String], parameters: Map[String, Map[String, Double]], top_n: Int, ref_matching_pairs: List[(EvaluationMatchingTask, File)], separated: Boolean): Option[(Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)] = {

    //Get Parameters
    val pre_pro_param_config: List[Map[String, Double]] = PARAM_CONFIGS_PRE_PRO.get(name).get

    val pre_pro_size = pre_pro_param_config.size
    logger.info(s"Start Outlier analysis for $name of size $pre_pro_size for the following processes" + process_files.toString())
    val results: Seq[Option[(Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)]] = pre_pro_param_config.zipWithIndex.map(config => {
      try {
        val run_number =  config._2
        val parameter_config: Map[String, Map[String, Double]] = parameters.+(name -> config._1)

        val results = executeListOfNonSeparatedProcesses(ds_name, config._2, selection_function, name, process_files, parameter_config, top_n, ref_matching_pairs, separated)
        logger.info(s"Finished Outlier analysis run $run_number of $pre_pro_size  for $name" + process_files.toString())
        SummaryPDFFactory.createReportForExecutionRun(folder, name + "_run_" + config._2, results, parameter_config)
        Option((parameter_config, results))
      }
      catch {
        case e: Throwable => {

          logger.error("Execution error",e)

          Option.empty
        }
      }
    })

    val results_filtered = results.filter(_.isDefined).map(_.get)


    val best_result: Option[(Predef.Map[String, Predef.Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)] = if (results_filtered.size > 0) {
      Option(results_filtered.maxBy(_._2.overall_agg_best.macro_eval_res.f1Measure))
    } else {
      logger.error("Empty result check processes " + process_files.toString())
      Option.empty
    }
    // val best_result: (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated) = results_filtered.maxBy(_._2.overall_agg_best.macro_eval_res.f1Measure)
    best_result
  }

  def executeListOfNonSeparatedProcesses(ds_name: String, run_number: Int, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], name: String, process_files: List[String], parameters: Map[String, Map[String, Double]], top_n: Int, ref_matching_pairs: List[(EvaluationMatchingTask, File)], separated: Boolean): ProcessEvalExecutionResultsNonSeparated = {
    val results: Map[String, ProcessEvalExecutionResultNonSeparated] = process_files.map(process_file => {

      logger.info(s"Start processes for separated = $separated")
      if (separated) {
        process_file -> executeProcessSeparated(ds_name, run_number, selection_function, ref_matching_pairs, process_file, name, parameters, IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS)
      } else {
        process_file -> executeProcessNonSeparated(ds_name, run_number, selection_function, ref_matching_pairs, process_file, parameters, top_n, name)
      }

    }).toMap

    //get best result by macro f1-measure
    val overall_best_result: (String, ProcessEvalExecutionResultNonSeparated) = results.maxBy(_._2.best_agg_res.macro_eval_res.f1Measure)

    ProcessEvalExecutionResultsNonSeparated(separated, overall_best_result._2.best_agg_res, overall_best_result, results)
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

  /*########################################################################
                     Custom Config builders
 ########################################################################*/

  /**
   * Create selection method list
   * @return
   */
  def createSelectionBasedOnConfig(): Map[String, List[Map[String, Double]]] = {
    val methods = Config.loaded_config.getStringList("optimization.selection")

    val configs = methods.map(method => {
      method -> SELECTION_CONFIG.get(method)
    }).toList

    val processed_config: Map[String, List[Map[String, Double]]] = configs.filter(_._2.isDefined).map(tuple => tuple._1 -> tuple._2.get).toMap
    processed_config
  }

  /**
   * Creates a Algorithm selection based on the config in application conf
   * @return
   */
  def createOutlierAlgoBasedOnConfig(): Map[String, List[Map[String, Double]]] = {
    val outlier_algo = Config.loaded_config.getStringList("optimization.processes")

    val algo_configs: Map[String, List[Map[String, Double]]] = outlier_algo.map(algo => {
      algo -> CONFIG_BY_OUTLIER_METHOD.get(algo)
    }).toList.filter(_._2.isDefined).map(tuple => tuple._1 -> tuple._2.get).toMap

    algo_configs
  }
}
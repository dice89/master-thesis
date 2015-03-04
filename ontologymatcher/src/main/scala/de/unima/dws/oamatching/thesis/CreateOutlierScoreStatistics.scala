package de.unima.dws.oamatching.thesis

import java.io.File

import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationMatchingTask, EvaluationDataSetParser, EvaluationMatchingRunner}
import de.unima.dws.oamatching.pipeline.optimize.ParameterOptimizer
import de.unima.dws.oamatching.pipeline.{MatchingPipelineCore, MatchingSelector, ScoreNormalizationFunctions}
import org.apache.commons.math.stat.descriptive.moment.Mean
import org.apache.commons.math.stat.inference.ChiSquareTestImpl
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest

import scala.collection.immutable.Map
import scala.collection.parallel.immutable.ParSeq

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

object CreateOutlierScoreStatistics extends App with OutlierEvaluationProcessParser with SeparatedOptimization with EvaluationDataSetParser with NonSeparatedOptimization {
  RapidminerJobs.init()
  //MatcherRegistry.initLargeScale()
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
    "cblof_x_means" -> "oacode_cblof_unweighted_x_means.rmp"
    //"rnn"->"oacode_rnn.rmp"
  )

  /*val IMPLEMENTED_OUTLIER_METHODS_BY_NAME = Map(
   "lcdof_x_means" -> "oacode_ldcof_x_means.rmp"
 )*/


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
  val SELECTION_CONFIG = Map("greedy_rank_fuzzy_delta" -> FUZZY_DELTA_SELECTION, "greedy_rank_fuzzy_ratio" -> FUZZY_RATIO_SELECTION,"greedy_rank" -> GREEDY_SELECTION)
  //val SELECTION_CONFIG = Map("greedy_rank" -> GREEDY_SELECTION)

  val SELECTION_METHODS_BY_NAME: Map[String, (Double) => (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double]] = Map("greedy_rank"->MatchingSelector.greedyRankSelectorSimpleExp, "greedy_rank_fuzzy_delta" -> MatchingSelector.fuzzyGreedyRankSelectorDelta, "greedy_rank_fuzzy_ratio" -> MatchingSelector.fuzzyGreedyRankSelectorDelta)

  /*########################################################################
                       Data Set Config
   ########################################################################*/

  val DS_LOCATION_BY_NAME = Map("benchmarks"->"ontos/2014/benchmarks","conference"->"ontos/2014/conference","anatomy"->"ontos/2014/anatomy")

  val mean_computer = new Mean()
  val stdev_computer = new StandardDeviation()
  val ratio_fuzzy_selection = 1.20
  val delta_fuzzy_selection = 0.001


  //val matching_pairs: List[(File, File)] = MiscExperiments.getListOfMatchingRefPairs


  //val conf = parseConference("ontos/2014/conference")

  /*########################################################################
                        Test Area
  ########################################################################*/

  //val select_fct_test = MatchingSelector.fuzzyGreedyRankSelectorDelta(delta_fuzzy_selection)
 // val bench = parseBenchmarks("ontos/2014/benchmarks")
  //val new_matching_pairs: List[(EvaluationMatchingTask, File)] = getListofProblemMatchingTasks(bench, "matchings/benchmarks/matchings")
  //runAllPreproOneMethod("cblof_regular_x_means","thesisexperiments/outliereval/test",GREEDY_SELECTION.head, new_matching_pairs, select_fct_test,true)


  //runAllNonSeparatedForAllAlgos("thesisexperiments/outliereval", matching_pairs, select_fct )
  //runAllNonSeparated("rnn","thesisexperiments/outliereval", matching_pairs, select_fct)
  //runAllForAllAlgos("thesisexperiments/outliereval/separated", matching_pairs, select_fct,false,"ontos/2014/conference",true )
  //val processes: OutlierEvaluationProcessesBySepartation = parseOutlierEvaluationProcesses("../RapidminerRepo/OutlierEvaluationV2","oacode_cblof_unweighted_regular_db_scan.rmp")
  //executeProcessSeparated(select_fct,matching_pairs,"ontos/2014/conference",processes.separated.pca_fixed.head,PCA_FIXED_FOLDER,IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS)
  //runAllForAllAlgosForAllSltcFunctions("thesisexperiments/outliereval", new_matching_pairs,false)


  startOptimizationForGivenDataset("thesisexperiments/outliereval","conference")



  RapidminerJobs.quit()

  def startOptimizationForGivenDataset(base_folder:String,ds_name:String):Unit = {

    val ds_location = DS_LOCATION_BY_NAME.get(ds_name).getOrElse("ontos/2014/conference")
    println(ds_location)
    val problems = parseProblems(ds_name,ds_location)
    val base_matchings_folder = "matchings"+File.separator+ds_name
    val base_folder_file = new File(base_matchings_folder)

    if(!base_folder_file.exists()){

      base_folder_file.mkdir()
      createFolder(base_matchings_folder+"/matchings")
      problems.foreach(task => {
        val feature_vector = MatchingPipelineCore.createFeatureVector(task.matching_problem,0.5,true)
        RapidminerJobs.writeCSV(task.matching_problem.name,base_matchings_folder)(feature_vector)
      })
    }

    val eval_folder_name =base_folder+File.separator+ds_name
    createFolder(eval_folder_name)
    val data_set = parseProblems(ds_name,"ontos/2014/"+ds_name)
    val matching_pairs: List[(EvaluationMatchingTask, File)] = getListofProblemMatchingTasks(data_set, base_matchings_folder+File.separator+"matchings")

    runAllForAllAlgosForAllSltcFunctions(ds_name,eval_folder_name,matching_pairs,true)
  }



  def runAllForAllAlgosForAllSltcFunctions(ds_name:String,base_folder: String, matching_pairs:List[(EvaluationMatchingTask, File)], parallel: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {

    createFolder(base_folder)




    val non_separated_folder = base_folder +"/non_separated"
    createFolder(non_separated_folder)
    val non_separated_best = runAllForAlgosForAllSlctFunctions(ds_name,non_separated_folder, matching_pairs, parallel, false)

    val separated_folder = base_folder +"/separated"
    createFolder(separated_folder)
    val separated_best = runAllForAlgosForAllSlctFunctions(ds_name,separated_folder, matching_pairs, parallel,  true)

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


  def runAllForAlgosForAllSlctFunctions(ds_name:String,base_folder: String, matching_pairs: List[(EvaluationMatchingTask, File)], parallel: Boolean, separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {
    val results = SELECTION_CONFIG.par.map { case (name, fuzzy_values) => {

      println("Start " +name)
      val fuzzy_base_folder = base_folder + "/" + name
      createFolder(fuzzy_base_folder)
      val selct_fct = SELECTION_METHODS_BY_NAME.get(name).get
      runAllForAlgosForOneFuzzySelectFct(ds_name,name, fuzzy_values, selct_fct, fuzzy_base_folder, matching_pairs, parallel, separated)
    }
    }

    val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = results.maxBy(_._2._2.overall_agg_best.macro_eval_res.f1Measure)
    HistogramChartFactory.createExecutionSummaryReport(base_folder, "best_result", best_result)

    best_result
  }


  def runAllForAlgosForOneFuzzySelectFct(ds_name:String,method: String, fuzzy_values: List[Map[String, Double]], slct_fct: (Double) => (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], base_folder: String, matching_pairs: List[(EvaluationMatchingTask, File)], parallel: Boolean, separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {


    val results = fuzzy_values.zipWithIndex.map(fuzzy_config => {
      val fuzzy_base_folder = base_folder + "/" + fuzzy_config._2
      createFolder(fuzzy_base_folder)

      val init_slct_fct = slct_fct(fuzzy_config._1.get("fuzzy").get)
      val name_to_config = Map(method -> 1.0)
      val final_config = fuzzy_config._1 ++ name_to_config

      val result = runAllForAllAlgos(ds_name,fuzzy_base_folder, matching_pairs, final_config, init_slct_fct, parallel, separated)


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
  def runAllForAllAlgos(ds_name:String,base_folder: String, matching_pairs: List[(EvaluationMatchingTask, File)], config: Map[String, Double], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], parallel: Boolean, separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {

    //createFolder(base_folder)
    val results = IMPLEMENTED_OUTLIER_METHODS_BY_NAME.map { case (name, files) => {

      runAllPreproOneMethod(ds_name, name, base_folder, config, matching_pairs, select_fct, separated)
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
  def runAllPreproOneMethod(ds_name:String,outlier_method: String, base_folder: String, select_config: Map[String, Double], matching_pairs: List[(EvaluationMatchingTask, File)], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double],  separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {
    createFolder(base_folder)
    val processes: OutlierEvaluationProcessesBySepartation = parseOutlierEvaluationProcesses("../RapidminerRepo/OutlierEvaluationV2", IMPLEMENTED_OUTLIER_METHODS_BY_NAME.get(outlier_method).get)

    //check if folder exists
    val base_folder_name: String = base_folder + "/" + outlier_method
    createFolder(base_folder_name)
    val configuration_list = CONFIG_BY_OUTLIER_METHOD.get(outlier_method).get


    println("Size of configs" + configuration_list.size)
    val results = configuration_list.zipWithIndex.map{case (config,index) => {
      val base_folder_config = base_folder_name + s"/$index"
      createFolder(base_folder_config)
      //just for reporting purposes
      val total_config = config ++ select_config
      val best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = runAlgorithmSingleNonSeparated(ds_name,index,processes, matching_pairs, select_fct, base_folder_config, total_config, separated)

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
  def runAlgorithmSingleNonSeparated(ds_name:String,run_number:Int,processes: OutlierEvaluationProcessesBySepartation, matching_pairs: List[(EvaluationMatchingTask, File)], select_fct: (Predef.Map[MatchRelation, Double], Double) => Predef.Map[MatchRelation, Double], base_folder_name: String, config: Map[String, Double], separated: Boolean): (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)) = {
    val param = Map("mining" -> config)

    val results = PRE_PRO_TECHNIQUES.map(pre_pro_name => {
      //distinguish between different parts

      println(pre_pro_name)
      val result: Option[(String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))] = if (pre_pro_name.equals(PCA_VARIANT_FOLDER)) {
        val folder_name = base_folder_name + "/" + PCA_VARIANT_FOLDER
        createFolder(folder_name)
        if (!separated) {
          Option((pre_pro_name, runNonSeparated(ds_name,run_number,folder_name, select_fct, pre_pro_name, processes.non_separated.pca_variance, param, 9, matching_pairs, separated)))
        } else {
          Option((pre_pro_name, runNonSeparated(ds_name,run_number,folder_name, select_fct, pre_pro_name, processes.separated.pca_variance, param,  9, matching_pairs, separated)))
        }
      } else if (pre_pro_name.equals(PCA_FIXED_FOLDER)) {
        val folder_name = base_folder_name + "/" + PCA_FIXED_FOLDER
        createFolder(folder_name)
        if (!separated) {
          Option((pre_pro_name, runNonSeparated(ds_name,run_number,folder_name, select_fct, pre_pro_name, processes.non_separated.pca_fixed, param, 9, matching_pairs, separated)))
        } else {
          Option((pre_pro_name, runNonSeparated(ds_name,run_number,folder_name, select_fct, pre_pro_name, processes.separated.pca_fixed, param, 9, matching_pairs, separated)))
        }
      } else if (pre_pro_name.equals(REMOVE_CORR_FOLDER)) {


        val folder_name = base_folder_name + "/" + REMOVE_CORR_FOLDER
        createFolder(folder_name)
        if (!separated) {
          Option((pre_pro_name, runNonSeparated(ds_name,run_number,folder_name, select_fct, pre_pro_name, processes.non_separated.remove_correlated, param,  9, matching_pairs, separated)))
        } else {
          Option((pre_pro_name, runNonSeparated(ds_name,run_number,folder_name, select_fct, pre_pro_name, processes.separated.remove_correlated, param,  9, matching_pairs, separated)))
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

  def runNonSeparated(ds_name:String,run_number:Int,folder: String, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], name: String, process_files: List[String], parameters: Map[String, Map[String, Double]], top_n: Int, ref_matching_pairs:List[(EvaluationMatchingTask, File)], separated: Boolean): (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated) = {

    //Get Parameters
    val pre_pro_param_config: List[Map[String, Double]] = PARAM_CONFIGS_PRE_PRO.get(name).get


    val results: Seq[Option[(Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated)]] = pre_pro_param_config.zipWithIndex.map(config => {
      try {
        val parameter_config: Map[String, Map[String, Double]] = parameters.+(name -> config._1)

        val results = executeListOfNonSeparatedProcesses(ds_name,config._2, selection_function, name, process_files, parameter_config,top_n, ref_matching_pairs, separated)
        println("finshed round " + config._2)
        HistogramChartFactory.createReportForExecutionRun(folder, name+"_run_"+config._2, results, parameter_config)
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

  def executeListOfNonSeparatedProcesses(ds_name:String,run_number:Int, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], name: String, process_files: List[String], parameters: Map[String, Map[String, Double]], top_n: Int, ref_matching_pairs:List[(EvaluationMatchingTask, File)], separated: Boolean): ProcessEvalExecutionResultsNonSeparated = {


    val results: Map[String, ProcessEvalExecutionResultNonSeparated] = process_files.map(process_file => {

      println("separated ? " +separated)
      if (separated) {
        process_file -> executeProcessSeparated(ds_name,run_number,selection_function, ref_matching_pairs, process_file, name, parameters, IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS)
      } else {
        process_file -> executeProcessNonSeparated(ds_name,run_number,selection_function, ref_matching_pairs, process_file, parameters, top_n, name)
      }

    }).toMap

    //get best result by macro f1-measure
    val overall_best_result: (String, ProcessEvalExecutionResultNonSeparated) = results.maxBy(_._2.best_agg_res.macro_eval_res.f1Measure)
    //println(overall_best_result)
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

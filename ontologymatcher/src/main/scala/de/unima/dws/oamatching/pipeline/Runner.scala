package de.unima.dws.oamatching.pipeline

import java.io.File

import de.unima.alcomox.ontology.IOntology
import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationMatchingRunner, EvaluationMatchingTask, EvaluationMatchingTaskWithParameters}
import de.unima.dws.oamatching.pipeline.registry.OutlierRegistry
import de.unima.dws.oamatching.pipeline.util.{TimeTaker, MetaDataMgmt}

import scala.collection.JavaConversions._

/**
 * Created by mueller on 28/01/15.
 */
case class RunConfiguration(threshold: Double, class_threshold:Double, dp_threshold:Double, op_threshold:Double,normalization: String, data_set_name: String, path_to_dataset: String, separated:Boolean,matching_pipline: (MatchingProblem, Double, Double) => (Alignment, FeatureVector),matching_pipline_separated: (MatchingProblem, Double, Double,Double,Double) => (Alignment, FeatureVector))

object Runner {
  //init stuff
  MatcherRegistry.initLargeScale()
  RapidminerJobs.init()


  /**
   * Runs a single structural Matcher based on a base matcher result
   * @param struct_matcher_name
   * @param base_matcher_name
   * @param onto1
   * @param onto2
   * @param ref
   * @param threshold
   * @param problem_name
   */
  def runSingleStructural(struct_matcher_name: String, base_matcher_name: String, onto1: String, onto2: String, ref: String, threshold: Double, problem_name: String): Unit = {

    val (reference: Alignment, test_problem: MatchingProblem) = prepareDataSet(onto1, onto2, ref, problem_name)

    val base_element_matcher = MatcherRegistry.getMatcherByName(base_matcher_name).get

    //base matcher threshold
    val base_threshold = 0.0

    val initial_Alignment = base_element_matcher.align(test_problem, base_threshold)

    val base_struct_matcher = MatcherRegistry.structural_matcher_by_name.get(struct_matcher_name).get
    val enhanced_Alignment = base_struct_matcher.align(test_problem, initial_Alignment, threshold)

    println(initial_Alignment.evaluate(reference))
    println(enhanced_Alignment.evaluate(reference))

  }


  /**
   * Runs the whole platform on a specified dataset
   * @param config
   */
  def runRound(config: RunConfiguration): Unit = {
    TimeTaker.takeTime("pipeline_and_evaluate")
    EvaluationMatchingRunner.matchAndEvaluateConference(Config.PATH_TO_CONFERENCE, config)
    val total = TimeTaker.takeTime("pipeline_and_evaluate")

    println(total)
  }

  /**
   * Runs the platform on one particular matching problem
   * @param onto1
   * @param onto2
   * @param ref
   * @param runConfiguration
   */
  def runSinglePlatform(onto1: String, onto2: String, ref: String, runConfiguration: RunConfiguration): Unit = {
    val (reference: Alignment, test_problem: MatchingProblem) = prepareDataSet(onto1, onto2, ref, runConfiguration.data_set_name)

    val task = EvaluationMatchingTaskWithParameters(test_problem, runConfiguration, reference)
    val result = EvaluationMatchingRunner.matchAndEvaluateSingle(task)
    println("pipeline res")
    println(result.evaluationResult)
    println("best base")
    println(result.bestBaseMatcher)
    println("majority vote")
    println(result.majorityVoteResult)

  }


  /**
   * Runs a single base matcher for a given problem and parameters
   * @param matcher_name
   * @param onto1
   * @param onto2
   * @param ref
   * @param threshold
   */
  def runSingleBaseMatcher(matcher_name: String, onto1: String, onto2: String, ref: String, threshold: Double, problem_name: String): Unit = {
    val (reference: Alignment, test_problem: MatchingProblem) = prepareDataSet(onto1, onto2, ref, problem_name)
    val o_matcher = MatcherRegistry.matcher_by_name(matcher_name)
    val o_threshold = MetaDataMgmt.getThreshold(test_problem.name, matcher_name).getOrElse(threshold)

    //check if threshold available
    val produced_alignment = o_matcher.align(test_problem, o_threshold)
    val result = produced_alignment.evaluate(reference)
    println(result)
  }

  /**
   * Runs a base matcher for all problems and aggregates the results
   * @param matcher_name
   * @param problems
   * @param threshold fallback threshold
   * @param problem_name
   */
  def runSingleBaseMatcherForMultipleProblems(matcher_name: String, problems: Vector[EvaluationMatchingTask], threshold: Double, problem_name: String): AggregatedEvaluationResult = {
    val matcher = MatcherRegistry.matcher_by_name(matcher_name)

    val base_threshold = MetaDataMgmt.getThreshold(problem_name, matcher_name).getOrElse(threshold)

    val results = problems.map(task => {
      val alignment = matcher.align(task.matching_problem, base_threshold)
      val res = alignment.evaluate(task.reference)

      res
    });

    val agg_res = EvaluationResultAggregator.aggregateEvaluationResults(results.toList)
    println(agg_res)
    agg_res
  }


  /**
   * Returns a matching problem for a
   * @param onto1
   * @param onto2
   * @param ref
   * @param data_set_name
   * @return
   */
  def prepareDataSet(onto1: String, onto2: String, ref: String, data_set_name: String): (Alignment, MatchingProblem) = {
    val file_onto1: File = new File(onto1)
    val file_onto2: File = new File(onto2)
    val reference = AlignmentParser.parseRDF(ref)

    val l_onto1 = OntologyLoader.load_fast_ontology(file_onto1.getPath)
    val l_onto2 = OntologyLoader.load_fast_ontology(file_onto2.getPath)
    val i_onto1 = new IOntology(file_onto1.getPath)
    val i_onto2 = new IOntology(file_onto2.getPath)
    val test_problem = MatchingProblem(l_onto1, l_onto2,i_onto1,i_onto2, data_set_name)
    (reference, test_problem)
  }


  def runEvaluateFromRapidminerFile(path: String, ref_file: String, threshold: Double): Unit = {
    val file: File = new File(path)
    val matchings_and_dim = RapidminerJobs.readCSV(file)
    val matchings = matchings_and_dim._3
    val selected = MatchingSelector.greedyRankSelectorSimple(matchings, threshold)
    selected.foreach(matching => println(matching._1))
    val alignment = new Alignment(null, null, selected)


    alignment.correspondences.foreach(cell => println(cell))
    val reference = AlignmentParser.parseRDF(ref_file)

    println(alignment.evaluate(reference))

  }


  def parseRunConfig(): RunConfiguration = {


    val threshold = Config.loaded_config.getDouble("pipeline.threshold")
    val class_threshold = Config.loaded_config.getDouble("pipeline.class_threshold")
    val dp_threshold = Config.loaded_config.getDouble("pipeline.dp_threshold")
    val op_threshold = Config.loaded_config.getDouble("pipeline.op_threshold")

    val norm_technique = Config.loaded_config.getString("pipeline.norm")
    val data_set = Config.loaded_config.getString("pipeline.dataset_type")
    val path_to_data_set = Config.loaded_config.getString("pipeline.path_to_dataset")

    val process_type = Config.loaded_config.getString("pipeline.outlier_method")
    val separated = Config.loaded_config.getBoolean("pipeline.separated")
    val pre_pro = Config.loaded_config.getString("pipeline.prepro.type")

    val rapidminerProcess = if (pre_pro.equals("pca_variant")) {
      OutlierRegistry.getProcessPCAVariant(process_type, separated)
    } else if (pre_pro.equals("pca_fixed")) {
      OutlierRegistry.getProcessPCAFixed(process_type, separated)
    } else if (pre_pro.equals("remove_corr")) {
      OutlierRegistry.getProcessRemoveCorrelated(process_type, separated)
    } else {
      //default
      OutlierRegistry.getProcessPCAVariant(process_type, separated)
    }

    if (rapidminerProcess.isDefined) {

    } else {
      // throw error
    }

    val mining_params: Map[String, Double] = Config.loaded_config.getObject("pipeline.mining").unwrapped().map(tuple => tuple._1 -> tuple._2.toString.toDouble).toMap
    val pre_pro_params: Map[String, Double] = Config.loaded_config.getObject("pipeline.prepro.values").unwrapped().map(tuple => tuple._1 -> tuple._2.toString.toDouble).toMap
    //build pipeline

    val parameters: Map[String, Map[String, Double]] = Map("mining" -> mining_params, pre_pro -> pre_pro_params)

    val outlier_function = RapidminerJobs.rapidminerOutlierDetection(rapidminerProcess.get, Config.loaded_config.getString("rapidminerconfig.tmp"), process_type, parameters, pre_pro) _
    val outlier_function_separated = RapidminerJobs.rapidminerOutlierDetectionSeparated(rapidminerProcess.get, Config.loaded_config.getString("rapidminerconfig.tmp"), process_type, parameters, pre_pro) _
    val norm_function = ScoreNormalizationFunctions.getNormFunction(norm_technique)
    val matching_pipline: (MatchingProblem, Double, Double) => (Alignment, FeatureVector) = MatchingPipelineCore.createMatchingPipeline(outlier_function)(norm_function)
    val matching_pipline_separated: (MatchingProblem, Double, Double,Double,Double) => (Alignment, FeatureVector) = MatchingPipelineCore.createMatchingPipelineSeparated(outlier_function_separated)(norm_function)

    RunConfiguration(threshold,class_threshold,dp_threshold,op_threshold, norm_technique, data_set, path_to_data_set, separated,matching_pipline,matching_pipline_separated)
  }


}
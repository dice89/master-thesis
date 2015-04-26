package de.unima.dws.oamatching.thesis

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.oamatching.analysis.{RapidminerJobs, SeparatedResults}
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.pipeline.ScoreNormalizationFunctions
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationMatchingRunner, EvaluationMatchingTask}
import de.unima.dws.oamatching.pipeline.optimize.ParameterOptimizer

import scala.collection.JavaConversions._
import scala.collection.immutable.Map
import scala.collection.parallel.{ForkJoinTaskSupport, ParIterable}

/**
 * Created by mueller on 27/02/15.
 */
trait SeparatedOptimization extends ResultHandling with LazyLogging with OptimizationDebugging {

  val csv_file = new File("tmp/separated_results.csv")
  val csv_result_writer = CSVWriter.open(csv_file)

  val parallel_degree = Config.loaded_config.getInt("pipeline.max_threads")

  def executeProcessSeparated(ds_name: String, run_number: Int, selection_function: (Map[MatchRelation, Double], Double,FastOntology, FastOntology) => Map[MatchRelation, Double], ref_matching_pairs: List[(EvaluationMatchingTask, File)], rapidminer_file: String, pre_pro_key: String, parameters: Map[String, Map[String, Double]], processes: Map[String, String]): ProcessEvalExecutionResultNonSeparated = {

    val process_name = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.lastIndexOf("."));
    val process_name_with_ending = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.size);
    //val process_type: String = IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS.get(process_name_with_ending).get
    val process_type: String = processes.get(process_name_with_ending).get


    logger.info(s"Start threshold optimization for $ds_name and $process_name in run $run_number")



    //execute Outlier analysis
    val matching_results = parallelizeCollection(ref_matching_pairs).map { case (ref_file, matching_file) => {
      try {

      } catch {
        case e: Throwable => {
          println(e)
          Option.empty
        }
      }
      val ref_alignment: Alignment = ref_file.reference
      //build different reference alignments out of original one for different classes
      val result: SeparatedResults = RapidminerJobs.rapidminerOutlierDetectionExperimentsSeparated(run_number, rapidminer_file, matching_file, parameters, pre_pro_key, process_type)
      println("done with " + ref_alignment.onto1 + ref_alignment.onto2 + "size classes: " + result.class_matchings._3.size + "size dp: " + result.dp_matchings._3.size + "size op: " + result.op_matchings._3.size)
      Option((result, ref_alignment))
    }
    }

    val size = matching_results.filter(!_.isDefined).size

    val matching_results_filtered = matching_results.filter(_.isDefined).map(_.get).seq
    println(s"Failure Matchings $size")

    println("matching done")
    //get usage probability for each parameter
    val probablility_of_usage: Map[String, Double] = getParameterUsageProbabilites(matching_results_filtered.toSeq)

    println("probability computation done")
    try {
      writeProbabilitiesToFile(ds_name, process_type, pre_pro_key, true, run_number, probablility_of_usage)

    } catch {
      case e: Throwable => {
        logger.error("error in proabilities", e)
      }
    }
    //write propbility of parameter usage to file

    println(probablility_of_usage)
    //normalize values
    val normalized_per_category: List[(Predef.Map[String, Predef.Map[MatchRelation, Double]], Predef.Map[String, Predef.Map[MatchRelation, Double]], Predef.Map[String, Predef.Map[MatchRelation, Double]], Alignment)] = matching_results_filtered.map { case (result, ref_alignment) => {

      //build different reference alignments out of original one for different classes
      val class_normalized = getNormalizedScores(result.class_matchings)
      val dp_normalized = getNormalizedScores(result.dp_matchings)
      val op_normalized = getNormalizedScores(result.op_matchings)

      (class_normalized, dp_normalized, op_normalized, (ref_alignment))
    }
    }.toList



    val grid_size = Config.loaded_config.getInt("optimization.threshold_opt.grid_size")
    val start = Config.loaded_config.getDouble("optimization.threshold_opt.start")
    val end = Config.loaded_config.getDouble("optimization.threshold_opt.end")
    val debug = Config.loaded_config.getBoolean("optimization.threshold_opt.debug")

    val optimization_grid = ParameterOptimizer.getDoubleGrid(start, end, grid_size)


    val optimal_thresholds = findOptimalThresholds(selection_function, normalized_per_category, optimization_grid)


    //construct final matchings and evaluate
    val best_results = optimal_thresholds.map { case (norm_technique, (class_threshold, dp_threshold, op_threshold)) => {
      val eval_res = evaluateRound(norm_technique, selection_function, normalized_per_category, class_threshold, dp_threshold, op_threshold, debug, true)

      norm_technique -> eval_res
    }
    }


    val best_result = best_results.maxBy(res_by_norm => res_by_norm._2.result.macro_eval_res.f1Measure)

    val json_result = createJSONResultString(ds_name, process_type, pre_pro_key, true, best_result._2.result, parameters, createJSONThresholdStringSeparated(best_result._2))

    println(s"Best Result $best_result")
    sendResultToServer(json_result)
    logger.info(s"Done with threshold optimization for $ds_name and $process_name in run $run_number")
    ProcessEvalExecutionResultNonSeparated(true, best_result._2.result, null, null, null, null, best_result, best_results)

  }

  def getParameterUsageProbabilites(matching_results: Seq[(SeparatedResults, Alignment)]): Predef.Map[String, Double] = {
    //get unique parameters
    val attributes_per_matching = matching_results.map { case (result, ref_alignment) => {
      val attributes_used = result.class_matchings._2.map(_._1)

      attributes_used.toList

    }
    }.toList

    val unique_attributes = attributes_per_matching.flatten.distinct

    //count the occurence for each attributes

    val probablility_of_usage = unique_attributes.map(attribute => {
      val total_usage = attributes_per_matching.map(attributes_for_problem => {
        if (attributes_for_problem.contains(attribute)) {
          1
        } else {
          0
        }
      })
      if (total_usage.size > 0 && attributes_per_matching.size > 0) {
        attribute -> total_usage.sum.toDouble / attributes_per_matching.size.toDouble
      } else {
        attribute -> 0.0
      }
    }).toMap
    probablility_of_usage
  }

  /**
   *
   * @param test
   * @return
   */
  def getNormalizedScores(test: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double])): Map[String, Map[MatchRelation, Double]] = {
    val norm_res_gaussian: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGaussianScaling(test._1, test._2, test._3).toMap
    val norm_res_euclidean_max: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByMaxEuclideanDistance(test._1, test._2, test._3).toMap
    val norm_res_gamma: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGammaScaling(test._1, test._2, test._3).toMap
    val norm_res_znorm: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByZScore(test._1, test._2, test._3).toMap


    val norm_config_list = Config.loaded_config.getStringList("optimization.normalization")
    val resulting_matchings_pre_conf: Map[String, Map[MatchRelation, Double]] = Map(("none", test._3), ("gaussian", norm_res_gaussian), ("zscore", norm_res_znorm), ("gamma", norm_res_gamma), ("euclidean_max", norm_res_euclidean_max))


    //return results according to config
    norm_config_list.map(norm_tech => norm_tech -> resulting_matchings_pre_conf.get(norm_tech).get).toMap

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
  def selectAndEvaluate(selection_function: (Map[MatchRelation, Double], Double, FastOntology, FastOntology) => Map[MatchRelation, Double], class_matchings: Map[MatchRelation, Double], dp_matchings: Map[MatchRelation, Double], op_matchings: Map[MatchRelation, Double], class_threshold: Double, dp_threshold: Double, op_threshold: Double, ref_alignment: Alignment, debug: Boolean, verbose: Boolean, norm_technique: String, sourceOnto:FastOntology, targetOnto:FastOntology): EvaluationResult = {

    val unselected = class_matchings ++ dp_matchings ++ op_matchings

    val selected_classes: Map[MatchRelation, Double] = selection_function(class_matchings, class_threshold, sourceOnto, targetOnto)
    val selected_dps: Map[MatchRelation, Double] = selection_function(dp_matchings, dp_threshold, sourceOnto, targetOnto)
    val selected_ops: Map[MatchRelation, Double] = selection_function(op_matchings, op_threshold, sourceOnto, targetOnto)

    val selected: Map[MatchRelation, Double] = selected_classes ++ selected_dps ++ selected_ops

    val result = if (debug) {
      val eval_res = debugAndEvaluateSeparated(class_threshold, dp_threshold, op_threshold, unselected, ref_alignment, selected, norm_technique, verbose)


      eval_res
    } else {
      val alignment = new Alignment(ref_alignment.onto1, ref_alignment.onto2, selected)

      val test_res = alignment.evaluate(ref_alignment)
      //println(test_res.f1Measure + "for " + ref_alignment.onto1 + "-" +ref_alignment.onto2)
      test_res
    }

    if (verbose) {
      printResult(norm_technique, ref_alignment, result)
    }


    result
  }


  def evaluateRound(norm_technique: String, selection_function: (Map[MatchRelation, Double], Double, FastOntology, FastOntology) => Map[MatchRelation, Double], normalizedScores: List[(Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Alignment)], class_threshold: Double, dp_threshold: Double, op_threshold: Double, debug: Boolean, verbose: Boolean): SeparatedResult = {
    val unique_techniques = normalizedScores.head._1.keys.toVector
    val scores = if (debug) normalizedScores
    else {
      parallelizeCollection(normalizedScores)
    }
    //get single results
    val results = scores.map { case (class_matchings, dp_matchings, op_matchings, ref_alignment) => {

      val onto1 = ref_alignment.onto1_reference
      val onto2 = ref_alignment.onto2_reference
      val class_matchings_norm = class_matchings.get(norm_technique).get
      val dp_matchings_norm = dp_matchings.get(norm_technique).get
      val op_matchings_norm = op_matchings.get(norm_technique).get

      selectAndEvaluate(selection_function, class_matchings_norm, dp_matchings_norm, op_matchings_norm, class_threshold, dp_threshold, op_threshold, ref_alignment, debug, verbose, norm_technique, onto1, onto2)

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
  def findOptimalThresholds(selection_function: (Map[MatchRelation, Double], Double, FastOntology, FastOntology) => Map[MatchRelation, Double], normalizedScores: List[(Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Map[String, Map[MatchRelation, Double]], Alignment)], threshold_grid: List[Double]): Map[String, (Double, Double, Double)] = {

    val unique_techniques = normalizedScores.head._2.keys.toVector
    //optimize thresholds


    //first find best class threshold
    val unique_par = parallelizeCollection(unique_techniques)
    val best_class_thresholds_by_norm = unique_par.map(norm_technique => {
      val results_by_threshold = threshold_grid.map(threshold => {
        (threshold, evaluateRound(norm_technique, selection_function, normalizedScores, threshold, 1.1, 1.1, false, false))
      })
      val best_result = results_by_threshold.maxBy(_._2.result.macro_eval_res.f1Measure)

      norm_technique -> best_result._1
    }).toMap

    //find best dp threshold
    val best_dp_thresholds_by_norm = unique_par.map(norm_technique => {
      val class_threshold = best_class_thresholds_by_norm.get(norm_technique).get
      val results_by_threshold = threshold_grid.map(threshold => {

        (threshold, evaluateRound(norm_technique, selection_function, normalizedScores, class_threshold, threshold, 1.1, false, false))
      })
      val best_result = results_by_threshold.maxBy(_._2.result.macro_eval_res.f1Measure)

      norm_technique -> best_result._1
    }).toMap


    val best_op_thresholds_by_norm = unique_par.map(norm_technique => {
      val class_threshold = best_class_thresholds_by_norm.get(norm_technique).get
      val dp_threshold = best_dp_thresholds_by_norm.get(norm_technique).get
      val results_by_threshold = threshold_grid.map(threshold => {

        (threshold, evaluateRound(norm_technique, selection_function, normalizedScores, class_threshold, dp_threshold, threshold, false, false))
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


  def printResult(norm_technique: String, ref: Alignment, eval_res: EvaluationResult): Unit = {


    val onto1_splitted = ref.onto1.split("/")
    val ont1_name = if (onto1_splitted.length > 3) onto1_splitted(onto1_splitted.length - 2) else ref.onto1

    val onto2_splitted = ref.onto2.split("/")
    val ont2_name = if (onto2_splitted.length > 3) onto2_splitted(onto2_splitted.length - 2) else ref.onto2

    val row: List[String] = List(norm_technique, ont1_name + "-" + ont2_name, eval_res.precision.toString, eval_res.recall.toString, eval_res.f1Measure.toString, eval_res.truePositives.toString, eval_res.falsePositives.toString, eval_res.FalseNegatives.toString)
    csv_result_writer.writeRow(row)

    csv_result_writer.flush()

  }


  def parallelizeCollection[A](to_be_parallelized: Iterable[A]): ParIterable[A] = {
    val para_it = to_be_parallelized.par
    para_it.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parallel_degree))
    para_it
  }

}

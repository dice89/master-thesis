package de.unima.dws.oamatching.thesis
import java.io.File

import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.pipeline.{MatchingPruner, ScoreNormalizationFunctions}
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationMatchingTask, EvaluationMatchingRunner}
import de.unima.dws.oamatching.pipeline.optimize.ParameterOptimizer
import org.apache.commons.math.stat.inference.ChiSquareTestImpl
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest

import scala.Predef
import scala.collection.immutable.Map
import scala.collection.parallel.immutable.ParSeq

/**
 * Created by mueller on 01/03/15.
 */
trait NonSeparatedOptimization extends ResultServerHandling{
  this: CreateOutlierScoreStatistics.type =>

  /**
   * Runs one none separated outlier method for one algo config and one prepro config
   * @param selection_function
   * @param ref_matching_pairs
   * @param rapidminer_file
   * @param parameters
   * @param top_n
   * @param pre_pro_key
   * @return
   */
  def executeProcessNonSeparated(ds_name:String,run_number: Int, selection_function: (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double], ref_matching_pairs:List[(EvaluationMatchingTask, File)], rapidminer_file: String, parameters: Map[String, Map[String, Double]], top_n: Int, pre_pro_key: String): ProcessEvalExecutionResultNonSeparated = {

    val process_name = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.lastIndexOf("."));
    val process_name_with_ending = rapidminer_file.slice(rapidminer_file.lastIndexOf("/") + 1, rapidminer_file.size);


    val process_type: String = IMPLEMENTED_OUTLIER_METHODS_BY_PROCESS.get(process_name_with_ending).get

    val matching_results_intermediate: ParSeq[(Map[String, (Map[MatchRelation, Double], Alignment)], (String, Map[String, Seq[(MatchRelation, Double, Boolean)]]), OutlierEvalStatisticsObject)] = ref_matching_pairs.par.map { case (ref_file, matching_file) => {
      val name = matching_file.getName.slice(0, matching_file.getName.lastIndexOf("."));

      val result: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) = RapidminerJobs.rapidminerOutlierDetectionExperiments(run_number, rapidminer_file, matching_file, parameters, pre_pro_key, process_type)

      val ref_alignment = ref_file.reference

     // val norm_res_gaussian: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGaussianScaling(result._1, result._2, result._3).toMap
      val norm_res_euclidean_max: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByMaxEuclideanDistance(result._1, result._2, result._3).toMap
      val norm_res_gamma: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByGammaScaling(result._1, result._2, result._3).toMap
      val norm_res_znorm: Map[MatchRelation, Double] = ScoreNormalizationFunctions.normalizeByZScore(result._1, result._2, result._3).toMap

      //val resulting_matchings: Map[String, (Map[MatchRelation, Double], Alignment)] = Map(("none", (result._3, ref_alignment)), ("gaussian", (norm_res_gaussian, ref_alignment)), ("zscore", (norm_res_znorm, ref_alignment)), ("gammma", (norm_res_gamma, ref_alignment)), ("euclidean_max", (norm_res_euclidean_max, ref_alignment)))
      val resulting_matchings: Map[String, (Map[MatchRelation, Double], Alignment)] = Map(  ("zscore", (norm_res_znorm, ref_alignment)), ("gammma", (norm_res_gamma, ref_alignment)), ("euclidean_max", (norm_res_euclidean_max, ref_alignment)))


      val top_n_none: Seq[(MatchRelation, Double, Boolean)] = getTopNResults(result._3, top_n, ref_alignment)

      val top_n_euclidean: Seq[(MatchRelation, Double, Boolean)] = getTopNResults(norm_res_euclidean_max, top_n, ref_alignment)
      val top_n_gamma: Seq[(MatchRelation, Double, Boolean)] = getTopNResults(norm_res_gamma, top_n, ref_alignment)
      val top_n_z: Seq[(MatchRelation, Double, Boolean)] = getTopNResults(norm_res_znorm, top_n, ref_alignment)


      //val top_n_scores = name -> Map("none" -> top_n_none, "gaussian" -> top_n_gaussian, "euclidean" -> top_n_euclidean, "gamma" -> top_n_gamma, "zscore" -> top_n_z)
      val top_n_scores = name -> Map( "euclidean" -> top_n_euclidean, "gamma" -> top_n_gamma, "zscore" -> top_n_z)

      val values = result._3.values.toArray
      val statistics: OutlierEvalStatisticsObject = computeBaseStatisticsOverOutlierScores(values, name)

      (resulting_matchings, top_n_scores, statistics)
    }
    }

    logger.info(s"Start threshold optimization for $ds_name and $process_name in run $run_number")

    val matching_results_seq = matching_results_intermediate.seq.toList
    val matching_results: List[Map[String, (Map[MatchRelation, Double], Alignment)]] = matching_results_seq.map(_._1)

    val grid_size = Config.loaded_config.getInt("optimization.threshold_opt.grid_size")
    val start = Config.loaded_config.getDouble("optimization.threshold_opt.start")
    val end = Config.loaded_config.getDouble("optimization.threshold_opt.end")

    val optimization_grid = ParameterOptimizer.getDoubleGrid(start, end, grid_size)

    val threshold_optimized_values: ThresholdOptResult = findOptimalThresholds(selection_function, matching_results, optimization_grid)

    val statistics: List[OutlierEvalStatisticsObject] = matching_results_seq.unzip3._3
    val top_n_results: Map[String, Map[String, Seq[(MatchRelation, Double, Boolean)]]] = matching_results_seq.unzip3._2.toMap

    //get best normalization technique by max macro f1 measure
    val best_result: (String, (Double, AggregatedEvaluationResult)) = threshold_optimized_values.best_global_results.maxBy(_._2._2.macro_eval_res.f1Measure)

    val json_result = createJSONResultString(ds_name,process_type,pre_pro_key,false, best_result._2._2,parameters,createJSONThresholdStringNonSeparated(best_result._2._1))

    sendResultToServer(json_result)

    logger.info(s"Done with threshold optimization for $ds_name and $process_name in run $run_number")

    ProcessEvalExecutionResultNonSeparated(false, best_result._2._2, threshold_optimized_values, statistics, top_n_results, best_result, null, null)
  }

  def getTopNResults(result: Map[MatchRelation, Double], n: Int, ref_alignment: Alignment): Seq[(MatchRelation, Double, Boolean)] = {
    val res_sorted = result.toSeq.sortWith((tuple1, tuple2) => tuple1._2 > tuple2._2)
    val top = res_sorted.zipWithIndex.filter(zipped_tuple => zipped_tuple._2 <= n).unzip._1


    val result_tp: Seq[(MatchRelation, Double, Boolean)] = top.map { case (relation, score) => {

      val cell = MatchingCell(relation.left, relation.right, score, relation.relation, relation.owl_type,relation.match_type)

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

    val number_of_debuggers = results_by_techniques.size*threshold_grid.size*results_by_techniques.head._2.size
    println(s"Size to be debugged $number_of_debuggers")
    //optimize for each matching technique and find global optimum
    val global_results: Map[String, Seq[(Double, AggregatedEvaluationResult)]] = results_by_techniques.map { case (name, list_of_matchings) => {

      //try for all thresholds
      val results_by_threshold = threshold_grid.map(threshold => {
        val eval_res_single_list: Seq[EvaluationResult] = list_of_matchings.map(single_matchings => {
          val selected = selection_function(single_matchings._1, threshold)


          val starttime = System.currentTimeMillis()
          //TODO add alignment debugging
          val alignment = new Alignment(single_matchings._2.onto1, single_matchings._2.onto2,single_matchings._2.onto1_reference,single_matchings._2.onto2_reference,selected)

          val debugged = MatchingPruner.debugAlignment(alignment)

          //val eval_res_norm = alignment.evaluate(single_matchings._2)

          val eval_res_debugged = debugged.evaluate(single_matchings._2)

          /*if(eval_res_debugged.f1Measure >= eval_res_norm.f1Measure){
            println("YEEAAH")
          }else {
            println("NOOO")
          }*/
          val totaltime = System.currentTimeMillis()-starttime

          System.gc()

          println(s"Needed $totaltime to debug alignment of size "+alignment.correspondences.size)
          eval_res_debugged
        })
        val agg_res = EvaluationMatchingRunner.computeAggregatedResults(eval_res_single_list.toList)
        (threshold, agg_res)
      })
      (name, results_by_threshold.seq)
    }
    }.toMap

    //get best result
    val best_global_results: Map[String, (Double, AggregatedEvaluationResult)] = global_results.map { case (name, list_of_results) => {
      val best_result: (Double, AggregatedEvaluationResult) = list_of_results.maxBy(_._2.macro_eval_res.f1Measure)
      (name, best_result)
    }
    }


    //write alignments back
    /*println(best_global_results.size)
    println(best_global_results.head._2._1)
    val best_global_threshold =best_global_results.head._2._1
    val list_of_matchings = results_by_techniques.get(best_global_results.head._1).get

    list_of_matchings.zipWithIndex.foreach(single_matchings=> {

      val selected = selection_function(single_matchings._1._1, 0.0)
      val alignment = new Alignment(null, null, selected)

      val selected_threshold = selection_function(single_matchings._1._1, best_global_threshold)
      val alignment_best = new Alignment(null, null, selected_threshold)
      val unselected = new Alignment(null, null, single_matchings._1._1)


      AlignmentParser.writeRDF(alignment_best,"tmp/alignments/"+single_matchings._2+"-best.rdf")

      AlignmentParser.writeRDF(alignment,"tmp/alignments/"+single_matchings._2+"-selected.rdf")

      AlignmentParser.writeRDF(unselected,"tmp/alignments/"+single_matchings._2+"-unselected.rdf")

    })*/


    //End debugging


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

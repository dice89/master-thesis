package de.unima.dws.oamatching.pipeline.evaluation

import java.io.File

import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.pipeline.util.ResultLogger
import de.unima.dws.oamatching.pipeline.{MatchingPipelineCore, MatchingProblem, FeatureVector}


import scala.collection.immutable.Map

case class EvaluationRoundResult(createdAlignment: Alignment, evaluationResult: EvaluationResult, baseMatcherResults: Map[String, EvaluationResult], majorityVoteResult: EvaluationResult, bestBaseMatcher: (String, EvaluationResult), betBaseBestBaseMatcher: Boolean, betMajorityVote: Boolean)

/**
 * Created by mueller on 28/01/15.
 */
object Evaluation extends App {

  val file_onto1: File = new File("ontos/2014/conference/cmt.owl")
  val file_onto2: File = new File("ontos/2014/conference/Conference.owl")

  val onto1 = OntologyLoader.load(file_onto1)
  val onto2 = OntologyLoader.load(file_onto2)

  val test_problem = MatchingProblem(onto1, onto2, "test")
  val reference = AlignmentParser.parseRDF("ontos/2014/conference/reference-alignment/cmt-conference.rdf")
  val test = evaluate(MatchingPipelineCore.matchProblem)(test_problem, reference, Map(("threshold", 0.6)))

  println("pipeline")
  println(test.evaluationResult)
  println("best base")
  println(test.bestBaseMatcher)
  println("majority vote")
  println(test.majorityVoteResult)


  /**
   *
   * @param matchingFkt Core Pipeline Function
   * @param matching_problem
   * @param reference
   * @param params
   * @return
   */
  def evaluate(matchingFkt: (MatchingProblem, Map[String, Double]) => (Alignment, FeatureVector))(matching_problem: MatchingProblem, reference: Alignment, params: Map[String, Double]): EvaluationRoundResult = {
    //match core platform and store results

    val problem_name = matching_problem.name;
    val core_pipeline_res = matchingFkt(matching_problem, params)
    val created_alignment = core_pipeline_res._1
    val feature_vector = core_pipeline_res._2

    //calculate evaluation result of core platform
    val core_platform_result = evaluateMatcher(created_alignment, reference, problem_name,"core_platform")

    //calc and evaluate majority vote
    val majority_vote_alignment = calcMajorityVoteAlignment(feature_vector)
    val majority_vote_res = evaluateMatcher(majority_vote_alignment, reference, problem_name,"majority_vote")


    //calc base matcher results
    val base_matcher_alignment = calcAllBaseMatchersAlignments(feature_vector)
    val base_matcher_results = evaluateAllBaseMatcher(base_matcher_alignment, reference,problem_name)

    //get best base matcher
    val best_base_matcher = getBestBaseMatcher(base_matcher_results)
    ResultLogger.log_matcher_result(problem_name,"best_base_matcher",best_base_matcher._2)

    //calculate bet statements
    val bet_best_base_matcher = best_base_matcher._2.f1Measure < core_platform_result.f1Measure
    val bet_majority_vote_matcher = majority_vote_res.f1Measure < core_platform_result.f1Measure

    val round_res: EvaluationRoundResult =  EvaluationRoundResult(created_alignment, core_platform_result, base_matcher_results, majority_vote_res, best_base_matcher, bet_best_base_matcher, bet_majority_vote_matcher)

    //log round result
    ResultLogger.log(problem_name + ":" + round_res.toString)

    round_res
  }


  /**
   * Implements the majority vote heuristic as proposed in "Eckert, Kai and Meilicke, Christian and Stuckenschmidt, Heiner: Improving Ontology Matching Using Meta-level Learning"
   * @param featureVector featureVector from Matching core pipeline
   * @return alignment
   */
  def calcMajorityVoteAlignment(featureVector: FeatureVector): Alignment = {

    //build threshold map for all matcher
    val matcher_name_to_threshold: Map[String, Double] = featureVector.matcher_name_to_index.keys.map(key => (key, getBaseMatcherThreshold(key, featureVector.data_set_name))).toMap

    val final_relations: Map[MatchRelation, Map[String, Double]] = featureVector.transposed_vector.filter { case (match_relation, matcher_to_measure) => {
      //decide wether to add this matchrelation to the final result or not
      //get majority count
      val majority = (matcher_to_measure.size.toDouble / 2).ceil
      //count of matcher that fullfill the criteria (res >= threshold) and therefore
      val count_matcher_fullfilled = matcher_to_measure.count { case (matcher_name, res) => res >= matcher_name_to_threshold.get(matcher_name).getOrElse(1.0)}

      (count_matcher_fullfilled.toDouble > majority)
    }
    }

    //bring to a format suitable for alignment, similarity score is always 1.0
    val final_relation_simplified: Map[MatchRelation, Double] = final_relations.map { case (match_relation, res_map) => (match_relation, 0.0)}.toMap

    //create final alignment and return
    val final_alignment: Alignment = new Alignment(null, null, final_relation_simplified)

    final_alignment
  }

  /**
   * Get all base matcher alignments
   * @param featureVector Feature vector from the core matching platform
   * @return
   */
  def calcAllBaseMatchersAlignments(featureVector: FeatureVector): Map[String, Alignment] = {
    //filter base matcher for their threshold and build alignment and yield it to automatically return it
    for ((matcher, relations) <- featureVector.vector) yield {
      val threshold: Double = getBaseMatcherThreshold(matcher, "TODO");
      val filtered_relation: Map[MatchRelation, Double] = relations.filter { case (relation, measure) => measure >= threshold}

      (matcher, new Alignment(null, null, filtered_relation))
    }
  }

  /**
   * Evaluates a single alignment
   * @param alignment produced alignment
   * @param reference reference to best tested against
   * @return EvaluationResult
   */
  def evaluateMatcher(alignment: Alignment, reference: Alignment, dataset: String, matcher: String): EvaluationResult = {
    val eval_res = alignment.evaluate(reference)
    //logging
    ResultLogger.log_matcher_result(dataset, matcher, eval_res)
    eval_res
  }

  /**
   * Evaluates all base matcher alignment
   * @param alignments Map containing all base matcher alignment by key
   * @param reference reference to be tested against
   * @return Map of EvaluationResults
   */
  def evaluateAllBaseMatcher(alignments: Map[String, Alignment], reference: Alignment, dataset: String): Map[String, EvaluationResult] = {
    alignments.map { case (matcher, alignment) => (matcher, evaluateMatcher(alignment, reference, dataset, matcher))}
  }

  /**
   * Get Best base Matcher, based on F1 Measure
   * @param baseMatcherAlignments Map of base matcher by key
   * @return A tuple with the basematcher name and the evaluation result
   */
  def getBestBaseMatcher(baseMatcherAlignments: Map[String, EvaluationResult]): (String, EvaluationResult) = {
    val bestBaseMatcher: (String, EvaluationResult) = baseMatcherAlignments.maxBy(_._2.f1Measure)
    bestBaseMatcher
  }

  /**
   * get the stored best threshold for a base matcher in for specific dataset
   * @param matcher_name name of the basematcher
   * @param dataset_name name of the dataset
   * @return
   */
  def getBaseMatcherThreshold(matcher_name: String, dataset_name: String): Double = {
    // TODO
    0.8
  }
}
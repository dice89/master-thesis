package de.unima.dws.oamatching.thesis

import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.{Alignment, AlignmentParser, EvaluationResult, MatchRelation}
import de.unima.dws.oamatching.pipeline.MatchingPruner

/**
 * Created by mueller on 27/03/15.
 */
trait OptimizationDebugging {

  def debugAndEvaluate(threshold: Double, single_matchings: Map[MatchRelation, Double], ref: Alignment, selected: Map[MatchRelation, Double], name: String, verbose:Boolean): EvaluationResult = {
    val alignment = new Alignment(ref.onto1, ref.onto2, ref.onto1_reference, ref.onto2_reference, ref.i_onto1, ref.i_onto2, selected)

    val debugged = MatchingPruner.debugAlignment(alignment, single_matchings, threshold)

    val eval_res_debugged = debugged.evaluate(ref)
    val eval_res_normal = alignment.evaluate(ref)

    val improvement = eval_res_debugged.f1Measure - eval_res_normal.f1Measure

    val problem_name = ref.onto1 + "-" + ref.onto2
    val prob_name = problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name

    if (verbose) {
      val threshkey = Math.round(threshold*10000).toString
      //AlignmentParser.writeRDF(debugged, "tmp/alignments/" + prob_name + ".rdf")

      AlignmentParser.writeFalseNegativesToCSV(alignment, ref, problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name +threshkey)
      AlignmentParser.writeTruePositivesToCSV(alignment, ref, problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name +threshkey)
      AlignmentParser.writeFalsePositivesToCSV(alignment, ref, problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name +threshkey)

      AlignmentParser.writeFalseNegativesAnalysis(alignment, ref, prob_name +threshkey, selected, single_matchings, threshold)

    }

    eval_res_debugged
  }

  def debugAndEvaluateSeparated(class_threshold: Double,dp_threshold: Double,op_threshold: Double,  single_matchings: Map[MatchRelation, Double], ref: Alignment, selected: Map[MatchRelation, Double], name: String, verbose:Boolean): EvaluationResult = {

    val alignment = new Alignment(ref.onto1, ref.onto2, ref.onto1_reference, ref.onto2_reference, ref.i_onto1, ref.i_onto2, selected)

    //val debugged = MatchingPruner.debugAlignment(alignment, single_matchings, class_threshold,dp_threshold, op_threshold)
    val debugged = MatchingPruner.debugAlignment(alignment)

    val eval_res_debugged = debugged.evaluate(ref)
    val eval_res_normal = alignment.evaluate(ref)

    val improvement = eval_res_debugged.f1Measure - eval_res_normal.f1Measure


    if (verbose) {
      val problem_name = ref.onto1 + "-" + ref.onto2
      val prob_name = problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name
      AlignmentParser.writeRDF(debugged, "tmp/alignments/" + prob_name + ".rdf")

      AlignmentParser.writeFalseNegativesToCSV(alignment, ref, problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name)
      AlignmentParser.writeTruePositivesToCSV(alignment, ref, problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name)
      AlignmentParser.writeFalsePositivesToCSV(alignment, ref, problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name)

      AlignmentParser.writeFalseNegativesAnalysis(alignment, ref, prob_name, selected, single_matchings, 0.0)

    }

    eval_res_debugged
  }
}

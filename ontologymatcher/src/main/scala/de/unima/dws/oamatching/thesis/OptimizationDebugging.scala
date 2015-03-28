package de.unima.dws.oamatching.thesis

import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.{Alignment, AlignmentParser, EvaluationResult, MatchRelation}
import de.unima.dws.oamatching.pipeline.MatchingPruner

/**
 * Created by mueller on 27/03/15.
 */
trait OptimizationDebugging {

  def debugAndEvaluate(threshold: Double, single_matchings: Predef.Map[MatchRelation, Double], ref: Alignment, selected: Predef.Map[MatchRelation, Double], name: String): EvaluationResult = {
    val alignment = new Alignment(ref.onto1, ref.onto2, ref.onto1_reference, ref.onto2_reference, ref.i_onto1, ref.i_onto2, selected)

    val debugged = MatchingPruner.debugAlignment(alignment, single_matchings, threshold)

    val eval_res_debugged = debugged.evaluate(ref)
    val eval_res_normal = alignment.evaluate(ref)


    val improvement = eval_res_debugged.f1Measure - eval_res_normal.f1Measure

    if(eval_res_debugged.f1Measure >= eval_res_normal.f1Measure){
      println("improved " + improvement)
    }else {
      println("fucked " + improvement)
    }
    val problem_name = ref.onto1 + "-" + ref.onto2
    val prob_name = problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name

    if (Config.loaded_config.getBoolean("optimization.write_details")) {
      val threshkey = threshold.toString.replaceAll(".", "_")
      //AlignmentParser.writeRDF(debugged, "tmp/alignments/" + prob_name + ".rdf")

      AlignmentParser.writeFalseNegativesToCSV(debugged, ref, problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name)
      AlignmentParser.writeTruePositivesToCSV(debugged, ref, problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name)
      AlignmentParser.writeFalsePositivesToCSV(debugged, ref, problem_name.replaceAll("http:/", "").replaceAll("/", "") + "_" + name)

      AlignmentParser.writeFalseNegativesAnalysis(debugged, ref, prob_name, selected, single_matchings, threshold)

    }

    eval_res_debugged
  }
}

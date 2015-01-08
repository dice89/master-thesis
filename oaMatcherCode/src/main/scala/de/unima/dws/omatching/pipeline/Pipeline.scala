package de.unima.dws.omatching.pipeline

import java.io.File
import java.net.URI

import scala.collection.convert.Wrappers.JEnumerationWrapper
import scala.collection.immutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.{ Map => MutableMap }
import scala.collection.mutable.MutableList

import org.semanticweb.owl.align.Alignment
import org.semanticweb.owl.align.AlignmentProcess

import de.unima.dws.omatching.matcher.MatchRelation
import de.unima.dws.omatching.matcher.MatchRelationURI
import de.unima.dws.omatching.matcher.PostPrunedMatcher
import de.unima.dws.omatching.outlierdetection.RapidminerBasedOutlierDetection
import de.unima.dws.omatching.pipeline.metaMatcher.BestSelectOutlierMatchingAlignment
import de.unima.dws.omatching.pipeline.metaMatcher.MatrixSelectionMatchingAlignment
import de.unima.dws.omatching.pipeline.metaMatcher.SuperNaiveOutlierMatchingAlignment
import de.unima.dws.omatching.pipeline.util.RdfFileFilter
import de.uniman.dws.oamatching.logging.ResultLogger
import fr.inrialpes.exmo.align.impl.eval.PRecEvaluator
import fr.inrialpes.exmo.align.parser.AlignmentParser

case class EvaluationResult(precision: Double, recall: Double, fmeasure: Double, tp: Int, fptp: Int, fnfp: Int)

case class OutlierAnalysisResult(left: String, relation: String, right: String, outlierFactor: Double)

case class MatcherResult(metaResult: EvaluationResult, singleResult: Map[String, EvaluationResult])

case class MatchingProblem(ontology1: URI, ontology2: URI, reference: Alignment, name: String)

object Pipeline {

  MatcherRegistry.init

  val usage = """
    Usage:  [--onto1 String] [--onto2 String]  [--ref String]
  """
  def main(args: Array[String]): Unit = {
    /*
	 * TODO Argument matching
    if (args.length == 0) println(usage)
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "--onto1" :: value :: tail =>
                               nextOption(map ++ Map('maxsize -> value.toInt), tail)
        case "--onto2" :: value :: tail =>
                               nextOption(map ++ Map('minsize -> value.toInt), tail)
        case string :: opt2 :: tail if isSwitch(opt2) => 
                               nextOption(map ++ Map('infile -> string), list.tail)
        case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
      }
    }
    val options = nextOption(Map(),arglist)
    println(options)
    */
    /*
    val file_onto1: File = new File("ontos/2014/conference/cmt.owl");

    val file_onto2: File = new File("ontos/2014/conference/Conference.owl");
 
    val gs_align: String ="ontos/2014/conference/reference-alignment/cmt-conference.rdf"

    val onto1: URI = file_onto1.toURI()
    val onto2: URI = file_onto2.toURI()
    

    val eval_rest = match_and_evaluate(rapidminerTest(writeCSV)(readCSV))(combineMatchingsNaive)(validate(gs_align))(onto1, onto2,gs_align)
    
    println(eval_rest)
    
    */
    //delete raw matching files
    val matchings_folder: File = new File("matchings");

    for (file_name <- matchings_folder.list()) {
      new File("matchings/" + file_name).delete()
    }

    /* val file_onto1: File = new File("ontos/2014/conference/ekaw.owl");

    val file_onto2: File = new File("ontos/2014/conference/iasted.owl");

    val gs_align: String = "ontos/2014/conference/reference-alignment/ekaw-iasted.rdf"

    val onto1: URI = file_onto1.toURI()
    val onto2: URI = file_onto2.toURI()

    val res = match_and_evaluate(rapidminerTest(writeCSV("test"))(readCSV))(combineMatchingsBetter)(validate(gs_align))(onto1, onto2, gs_align)
*/
    //match_oaei_data_set("ontos/2014/conference")

    //optimizeThresholdBaseMachterGlobal(parse_conference_2014("ontos/2014/conference"), 10, "conference")

    
    optimizeMetaMatchingThreshold(parse_conference_2014)("ontos/2014/conference","conference",10)
    //matchAndEvaluateProblems(parse_conference_2014)("ontos/2014/conference", "conference", 0.5)
  }

  def optimizeThresholdBaseMatcherLocal(matcher_name: String, steps: Int, problem: MatchingProblem, dataset_name: String): Double = {
    val stepsize: Double = 1.0 / steps.asInstanceOf[Double]

    val results = for (index <- 0 until steps) yield {
      val threshold: Double = 1.0 - (stepsize * index)
      (threshold, MatcherRegistry.matchSingle(problem, matcher_name, threshold)._1)
    }
    val best_result = results.reduceLeft((B, A) => {

      if (B._2.fmeasure > A._2.fmeasure) {
        B
      } else {
        A
      }
    })

    // best threshold
    best_result._1
  }

  def optimizeThresholdSingleBaseMatcherGlobal(problems: Seq[MatchingProblem], matcher_name: String, steps: Int, dataset_name: String): Double = {
    val stepsize: Double = 1.0 / steps.asInstanceOf[Double]

    val results = for (
      index <- 0 until steps;
      threshold <- Option[Double](1.0 - (stepsize * index));
      if (threshold > 0.1)
    ) yield {
      println("Start Matcher " + matcher_name)
      val res = (threshold, MatcherRegistry.matchProblemsWithOneMatcherOptimizeOnly(problems, matcher_name, dataset_name, threshold))

      println("Matcher " + matcher_name + " " + res)
      Option(res)
    }
    val best_result = results.reduceLeft((B, A) => {
      if (B.isDefined && A.isDefined) {
        if (B.get._2.fmeasure > A.get._2.fmeasure) {
          B
        } else {
          A
        }
      } else {
        if (B.isDefined && A.isEmpty) {
          B
        } else {
          A
        }
      }

    })

    ResultLogger.log("Best Threshold for: " + matcher_name + " for dataset " + dataset_name + " is " + best_result.get._1)
    best_result.get._1
  }

  def optimizeThresholdBaseMachterGlobal(problems: Seq[MatchingProblem], steps: Int, dataset_name: String) = {
    for (matcher <- MatcherRegistry.matcher_by_name.keys) {
      optimizeThresholdSingleBaseMatcherGlobal(problems, matcher, steps, dataset_name)
    }
  }

  def parse_conference_2014(path_to_folder: String): Seq[MatchingProblem] = {
    val folder: File = new File(path_to_folder + File.separator + "reference-alignment/")
    val problems = for (ref_align_file <- folder.listFiles(new RdfFileFilter)) yield {
      val ontos: List[String] = ref_align_file.getName().split("-").toList
      val name_onto1: String = path_to_folder + File.separator + ontos(0).replaceAll("-", "") + ".owl"
      val name_onto2: String = path_to_folder + File.separator + ontos(1).replaceAll("-", "").replaceAll(".rdf", "") + ".owl"
      val onto1: URI = new File(name_onto1).toURI()
      val onto2: URI = new File(name_onto2).toURI()
      //parse alginments
      var aparser: AlignmentParser = new AlignmentParser();
      val reference: Alignment = aparser.parse(new File(ref_align_file.getAbsolutePath()).toURI());
      println(ref_align_file.getAbsolutePath())
      val name: String = ref_align_file.getName().dropRight(4)
      val alignments = new JEnumerationWrapper(reference.getElements()).toList;

      println(alignments.size)
      MatchingProblem(onto1, onto2, reference, name)
    }

    problems
  }

  def optimizeMetaMatchingThreshold(parsefunction: String => Seq[MatchingProblem])(path_to_folder: String, dataset_name: String, steps: Int): Double = {
    val stepsize: Double = 1.0 / steps.asInstanceOf[Double]

    val results = for (
      index <- 0 until steps;
      threshold <- Option[Double](1.0 - (stepsize * index));
      if (threshold > 0.1)
    ) yield {
      println("Start Matcher ")

      val res = (threshold, matchAndEvaluateProblems(parsefunction)(path_to_folder, dataset_name, threshold))

      println("Matcher " + res)
      Option(res)
    }

    val best_result = results.reduceLeft((B, A) => {
      if (B.isDefined && A.isDefined) {
        if (B.get._2.fmeasure > A.get._2.fmeasure) {
          B
        } else {
          A
        }
      } else {
        if (B.isDefined && A.isEmpty) {
          B
        } else {
          A
        }
      }

    })

    ResultLogger.log("Best Threshold metamatcher for for dataset " + dataset_name + " is " + best_result.get._1)

    best_result.get._1

  }

  def matchAndEvaluateProblems(parsefunction: String => Seq[MatchingProblem])(path_to_folder: String, dataset_name: String, threshold: Double): EvaluationResult = {
    val problems = parsefunction(path_to_folder)

    var base_matcher_results: MutableList[Map[String, EvaluationResult]] = new MutableList
    var meta_matcher_results: MutableList[EvaluationResult] = new MutableList
    for (problem <- problems) {
      val res = metaMatchAndEvaluate(RapidminerBasedOutlierDetection.rapidminerOutlierReadWrite(problem.name))(combineMatchingsMatrix)(validate(problem.reference))(problem, dataset_name, threshold)
      base_matcher_results += res.singleResult
      meta_matcher_results += res.metaResult

      ResultLogger.log_matcher_result(problem.name, "metaMatcher", res.metaResult)
    }

    //aggregate base matcher
    var aggregated_base_matcher: MutableMap[String, EvaluationResult] = new HashMap
    for (match_res <- base_matcher_results; single_match <- match_res) {
      if (aggregated_base_matcher.contains(single_match._1)) {
        //aggregate
        var result: EvaluationResult = aggregated_base_matcher.get(single_match._1).get

        val agg_precision: Double = (result.precision + single_match._2.precision)

        val agg_recall: Double = (result.recall + single_match._2.recall)

        val agg_fmeasure: Double = (result.fmeasure + single_match._2.fmeasure)
        aggregated_base_matcher.put(single_match._1, EvaluationResult(agg_precision, agg_recall, agg_fmeasure, 0, 0, 0))

      } else {
        aggregated_base_matcher.put(single_match._1, single_match._2)
      }
      //key 
    }

    aggregated_base_matcher = aggregated_base_matcher.map({
      case (name, evalRes) => {
        val size: Int = base_matcher_results.size
        val eval_res_normalized = EvaluationResult(evalRes.precision / size, evalRes.recall / size, evalRes.fmeasure / size, 0, 0, 0)
        (name, eval_res_normalized)
      }
    })
    //aggregated_base_matcher.max
    println(aggregated_base_matcher)

    println("Base Line 1: (best average Matcher)")
    //get best on average f1 measure base matcher
    val best_average_matcher = aggregated_base_matcher.reduceLeft((A, B) => {
      if (A._2.fmeasure > B._2.fmeasure) {
        A
      } else {
        B
      }
    })
    //log baseline 1
    ResultLogger.log_result(path_to_folder, "BaseLine 1: " + best_average_matcher._1, best_average_matcher._2)
    //aggregate single matcher results

    //Base Line 2: Get result based on best matcher for every matching

    val best_for_each_match = for (result_row <- base_matcher_results)
      yield result_row.reduceLeft((A, B) => {
      if (A._2.fmeasure < B._2.fmeasure) {
        B
      } else {
        A
      }
    })

    val best_summed: EvaluationResult = best_for_each_match.reduceLeft((A, B) => {
      ("res", EvaluationResult(A._2.precision + B._2.precision, A._2.recall + B._2.recall, A._2.fmeasure + B._2.fmeasure, 0, 0, 0))
    })._2

    val best_normalized = EvaluationResult(best_summed.precision / meta_matcher_results.size, best_summed.recall / meta_matcher_results.size, best_summed.fmeasure / meta_matcher_results.size, 0, 0, 0)

    //log baseline
    ResultLogger.log_result(path_to_folder, "BaseLine2", best_normalized)

    var summed_result: EvaluationResult = meta_matcher_results.foldLeft(EvaluationResult(0.0, 0.0, 0.0, 0, 0, 0))((z, i) => {
      EvaluationResult(z.precision + i.precision, z.recall + i.recall, z.fmeasure + i.fmeasure, z.tp + i.tp, z.fptp + i.fptp, z.fnfp + i.fnfp)
    })
    val combined_matcher_result = EvaluationResult(summed_result.precision / meta_matcher_results.size, summed_result.recall / meta_matcher_results.size, summed_result.fmeasure / meta_matcher_results.size, 0, 0, 0)

    println("Outlier Detection Meta Matcher Result")
    println("Micro Average")

    val recall: Double = summed_result.tp.toDouble / summed_result.fnfp.toDouble
    val precision: Double = summed_result.tp.toDouble / summed_result.fptp.toDouble
    val fmeasure: Double = 2 * precision * recall / (precision + recall);

    val micro_combined_result = EvaluationResult(recall, precision, fmeasure, summed_result.tp, summed_result.fptp, summed_result.fnfp)

    ResultLogger.log_result(path_to_folder, "Average Meta Matcher Micro ", micro_combined_result)
    println(micro_combined_result)
    println("Macro Average")

    println(combined_matcher_result)

    ResultLogger.log_result(path_to_folder, "Average Meta Matcher", combined_matcher_result)

    println("Bet Baseline 1? " + (best_average_matcher._2.fmeasure < combined_matcher_result.fmeasure))
    println("Bet Baseline 2? " + (best_normalized.fmeasure < combined_matcher_result.fmeasure))

    combined_matcher_result

  }

  def metaMatchAndEvaluate(outlierFunction: Map[MatchRelation, Map[String, Option[Double]]] => Map[(MatchRelationURI), Double])(combinationFunction: (Map[(MatchRelationURI), Double], Double) => AlignmentProcess)(evaluationFunction: AlignmentProcess => EvaluationResult)(problem: MatchingProblem, dataset_name: String, threshold: Double): MatcherResult = {

    val matchings = matchRoundWithResult(problem, dataset_name)
    val meta_result = evaluationFunction(combinationFunction(outlierFunction(matchings._1), threshold))

    MatcherResult(meta_result, matchings._2)
  }

  def matchRoundWithResult(problem: MatchingProblem, dataset_name: String): (Map[MatchRelation, Map[String, Option[Double]]], Map[String, EvaluationResult]) = {
    val results = MatcherRegistry.matcher_by_name.map({
      case (name, matcher) =>
        matchSingleWithResult(matcher, problem, name, dataset_name)
    }).toMap

    val unique_elements = results.map({ case (name, (eval, matchings)) => matchings.keySet }).flatten
    val results_per_matching = unique_elements.map(matching => matching -> results.filter(triple => triple._2._2.contains(matching)).map({ case (name, (eval, matchings)) => name -> matchings.get(matching) })).toMap
    val eval_map = results.map({ case (name, (eval, matchings)) => name -> eval });

    (results_per_matching, eval_map)

  }

  def matchSingleWithResult(matcher: PostPrunedMatcher, problem: MatchingProblem, matcher_name: String, dataset_name: String): (String, (EvaluationResult, Map[MatchRelation, Double])) = {
    matcher.prepare(problem)
    //get threshold --- if not set in redis default value is 0.9
    val threshold = ThresholdOptimizationPlatform.getThreshold(matcher_name, dataset_name).getOrElse(0.9)
    val matchings = matcher.align(threshold)
    val eval_res = matcher.evaluate

    //logging
    ResultLogger.log_matcher_result(dataset_name, matcher_name, eval_res)

    (matcher_name, (eval_res, matchings))
  }

  def combineMatchingsNaive(matchings: Map[MatchRelationURI, Double], threshold: Double): AlignmentProcess = {
    var alignment: AlignmentProcess = new SuperNaiveOutlierMatchingAlignment(matchings, threshold)

    alignment.align(null, null)
    alignment
  }

  def combineMatchingsBetter(matchings: Map[MatchRelationURI, Double], threshold: Double): AlignmentProcess = {
    var alignment: AlignmentProcess = new BestSelectOutlierMatchingAlignment(matchings, threshold)
    alignment.align(null, null)
    alignment
  }

  def combineMatchingsMatrix(matchings: Map[MatchRelationURI, Double], threshold: Double): AlignmentProcess = {
    var alignment: AlignmentProcess = new MatrixSelectionMatchingAlignment(matchings, threshold)
    alignment.align(null, null)
    alignment
  }

  def validate(reference_alignment: String)(alignment: AlignmentProcess): EvaluationResult = {
    var aparser: AlignmentParser = new AlignmentParser(0);
    var reference: Alignment = aparser.parse(new File(reference_alignment).toURI());
    var evaluator: PRecEvaluator = new PRecEvaluator(reference, alignment);
    evaluator.eval(null)

    EvaluationResult(evaluator.getPrecision(), evaluator.getRecall(), evaluator.getFmeasure(), 0, 0, 0)
  }

  def validate(reference: Alignment)(alignment: AlignmentProcess): EvaluationResult = {
    val evaluator: PRecEvaluator = new PRecEvaluator(reference, alignment);
    evaluator.eval(null)

    EvaluationResult(evaluator.getPrecision(), evaluator.getRecall(), evaluator.getFmeasure(), evaluator.getCorrect(), evaluator.getFound(), evaluator.getExpected())
  }

  /*
   def match_and_evaluate(outlierFunction: Map[MatchRelation, Map[String, Option[Double]]] => Map[(MatchRelationURI), Double])(combinationFunction: (Map[(MatchRelationURI), Double], Double) => AlignmentProcess)(evaluationFunction: AlignmentProcess => EvaluationResult)(onto1: URI, onto2: URI, ref_align: String, threshold: Double, prefix: String): MatcherResult = {
    MatcherRegistry.init

    var aparser: AlignmentParser = new AlignmentParser(0);
    var reference: Alignment = aparser.parse(new File(ref_align).toURI());
    val matchings = MatcherRegistry.matchRound(onto1, onto2, reference, prefix)

    val meta_result = evaluationFunction(combinationFunction(outlierFunction(matchings._1), threshold))

    MatcherResult(meta_result, matchings._2)
  }

  def match_and_evaluate_threshold_optimized(outlierFunction: Map[MatchRelation, Map[String, Option[Double]]] => Map[(MatchRelationURI), Double])(combinationFunction: (Map[(MatchRelationURI), Double], Double) => AlignmentProcess)(evaluationFunction: AlignmentProcess => EvaluationResult)(onto1: URI, onto2: URI, ref_align: String, prefix: String, steps: Int): MatcherResult = {
    MatcherRegistry.init

    var aparser: AlignmentParser = new AlignmentParser(0);
    var reference: Alignment = aparser.parse(new File(ref_align).toURI());
    val matchings = MatcherRegistry.matchRound(onto1, onto2, reference, prefix)
    //do parameter optimization
    val scored_matchings = outlierFunction(matchings._1)

    val stepsize: Double = 1.0 / steps.asInstanceOf[Double]

    val results = for (index <- 0 until steps) yield {
      val threshold: Double = 1.0 - (stepsize * index)
      val result = evaluationFunction(combinationFunction(scored_matchings, threshold))
      (threshold, result)
    }

    val meta_result: (Double, EvaluationResult) = results.toList.reduceLeft((A, B) => {
      if (A._2.fmeasure > B._2.fmeasure) {
        A
      } else {
        B
      }
    })

    // println("Result at threshold " + meta_result._1 + " for Meta Matcher" + meta_result._2)
    ResultLogger.log("Best Result for " + prefix + " :" + meta_result._1)
    MatcherResult(meta_result _2, matchings._2)
  }
 */
}
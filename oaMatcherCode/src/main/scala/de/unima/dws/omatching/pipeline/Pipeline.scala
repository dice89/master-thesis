package de.unima.dws.omatching.pipeline

import java.io.File
import java.net.URI
import java.util.Properties
import scala.collection.immutable.{ Map => ImmutableMap }
import scala.collection.mutable.Map
import org.semanticweb.owl.align.Alignment
import org.semanticweb.owl.align.AlignmentProcess
import org.semanticweb.owl.align.Evaluator
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter
import com.rapidminer.{ Process => RProcess }
import com.rapidminer.RapidMiner
import com.rapidminer.operator.io.CSVDataReader
import com.rapidminer.operator.io.CSVExampleSetWriter
import de.unima.dws.omatching.pipeline.metaMatcher.SuperNaiveOutlierMatchingAlignment
import fr.inrialpes.exmo.align.impl.eval.PRecEvaluator
import fr.inrialpes.exmo.align.parser.AlignmentParser
import de.unima.dws.omatching.matcher.MatchRelation
import de.unima.dws.omatching.matcher.MatchRelationURI
import de.unima.dws.omatching.matcher.MatchRelationURI
import de.unima.dws.omatching.matcher.MatchRelationURI
import de.unima.dws.omatching.matcher.MatchRelationURI
import de.unima.dws.omatching.matcher.MatchRelationURI
import de.unima.dws.omatching.matcher.MatchRelationURI
import de.unima.dws.omatching.pipeline.util.RdfFileFilter
import java.util.ArrayList
import scala.collection.mutable.MutableList
import scala.collection.mutable.HashMap
import de.unima.dws.omatching.pipeline.metaMatcher.OutlierMatchingCombinationAlignment
import de.unima.dws.omatching.pipeline.metaMatcher.BestSelectOutlierMatchingAlignment
import de.unima.dws.omatching.pipeline.metaMatcher.BestSelectOutlierMatchingAlignment
import de.unima.dws.omatching.pipeline.metaMatcher.MatrixSelectionMatchingAlignment
import de.uniman.dws.oamatching.logging.ResultLogger

case class EvaluationResult(precision: Double, recall: Double, fmeasure: Double)

case class OutlierAnalysisResult(left: String, relation: String, right: String, outlierFactor: Double)

case class MatcherResult(metaResult: EvaluationResult, singleResult: ImmutableMap[String, EvaluationResult])

object Pipeline {
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
    match_oaei_data_set("ontos/2014/conference")
  }

  def match_oaei_data_set(path_to_folder: String): Unit = {

    val folder: File = new File(path_to_folder + File.separator + "reference-alignment/")
    // println(folder.getAbsolutePath() + "--" + folder.isDirectory())
    var base_matcher_results: MutableList[ImmutableMap[String, EvaluationResult]] = new MutableList
    var meta_matcher_results: MutableList[EvaluationResult] = new MutableList
    for (ref_align_file <- folder.listFiles(new RdfFileFilter)) {
      val ontos: List[String] = ref_align_file.getName().split("-").toList
      val name_onto1: String = path_to_folder + File.separator + ontos(0).replaceAll("-", "") + ".owl"
      val name_onto2: String = path_to_folder + File.separator + ontos(1).replaceAll("-", "").replaceAll(".rdf", "") + ".owl"

      val onto1: URI = new File(name_onto1).toURI()
      val onto2: URI = new File(name_onto2).toURI()
      //match
      val csv_prefix: String = ref_align_file.getName().dropRight(4)

      val res = match_and_evaluate(rapidminerTest(writeCSV(csv_prefix))(readCSV))(combineMatchingsMatrix)(validate(ref_align_file.getAbsolutePath()))(onto1, onto2, ref_align_file.getAbsolutePath(),0.5)

      base_matcher_results.+=(res.singleResult)
      meta_matcher_results.+=(res.metaResult)
      
      ResultLogger.log_matcher_result(csv_prefix, "metaMatcher", res.metaResult )
    }

    //aggregate base matcher
    var aggregated_base_matcher: Map[String, EvaluationResult] = new HashMap
    for (match_res <- base_matcher_results; single_match <- match_res) {
      if (aggregated_base_matcher.contains(single_match._1)) {
        //aggregate
        var result: EvaluationResult = aggregated_base_matcher.get(single_match._1).get

        val agg_precision: Double = (result.precision + single_match._2.precision)

        val agg_recall: Double = (result.recall + single_match._2.recall)

        val agg_fmeasure: Double = (result.fmeasure + single_match._2.fmeasure)
        aggregated_base_matcher.put(single_match._1, EvaluationResult(agg_precision, agg_recall, agg_fmeasure))

      } else {
        aggregated_base_matcher.put(single_match._1, single_match._2)
      }
      //key 
    }

    aggregated_base_matcher = aggregated_base_matcher.map({
      case (name, evalRes) => {
        val size: Int = base_matcher_results.size
        val eval_res_normalized = EvaluationResult(evalRes.precision / size, evalRes.recall / size, evalRes.fmeasure / size)
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
    ResultLogger.log_result(path_to_folder, "BaseLine 1: " + best_average_matcher._1  , best_average_matcher._2 )
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
      ("res", EvaluationResult(A._2.precision + B._2.precision, A._2.recall + B._2.recall, A._2.fmeasure + B._2.fmeasure))
    })._2

    val best_normalized = EvaluationResult(best_summed.precision / meta_matcher_results.size, best_summed.recall / meta_matcher_results.size, best_summed.fmeasure / meta_matcher_results.size)

    //log baseline
    ResultLogger.log_result(path_to_folder, "BaseLine2", best_normalized)
    
    
    var summed_result: EvaluationResult = meta_matcher_results.foldLeft(EvaluationResult(0.0, 0.0, 0.0))((z, i) => {
      EvaluationResult(z.precision + i.precision, z.recall + i.recall, z.fmeasure + i.fmeasure)
    })
    val combined_matcher_result = EvaluationResult(summed_result.precision / meta_matcher_results.size, summed_result.recall / meta_matcher_results.size, summed_result.fmeasure / meta_matcher_results.size)

    println("Outlier Detection Meta Matcher Result")
    println(combined_matcher_result)
    
    ResultLogger.log_result(path_to_folder, "Average Meta Matcher", combined_matcher_result)
    
    println ("Bet Baseline 1? " + (best_average_matcher._2.fmeasure < combined_matcher_result.fmeasure) )
    println ("Bet Baseline 2? " + (best_normalized.fmeasure  < combined_matcher_result.fmeasure) )
    //base_matcher_results

  }

  def match_and_evaluate(outlierFunction: ImmutableMap[MatchRelation, ImmutableMap[String, Option[Double]]] => ImmutableMap[(MatchRelationURI), Double])(combinationFunction: (ImmutableMap[(MatchRelationURI), Double], Double) => AlignmentProcess)(evaluationFunction: AlignmentProcess => EvaluationResult)(onto1: URI, onto2: URI, ref_align: String,threshold:Double): MatcherResult = {
    MatcherRegistry.init

    var aparser: AlignmentParser = new AlignmentParser(0);
    var reference: Alignment = aparser.parse(new File(ref_align).toURI());
    val matchings = MatcherRegistry.matchRound(onto1, onto2, reference)
    val meta_result = evaluationFunction(combinationFunction(outlierFunction(matchings._1), threshold))
    println("Result for Meta Matcher" + meta_result)

    MatcherResult(meta_result, matchings._2)
  }

  def combineMatchingsNaive(matchings: ImmutableMap[MatchRelationURI, Double], threshold: Double): AlignmentProcess = {
    var alignment: AlignmentProcess = new SuperNaiveOutlierMatchingAlignment(matchings, threshold)

    alignment.align(null, null)
    alignment
  }

  def combineMatchingsBetter(matchings: ImmutableMap[MatchRelationURI, Double], threshold: Double): AlignmentProcess = {
    var alignment: AlignmentProcess = new BestSelectOutlierMatchingAlignment(matchings, threshold)
    alignment.align(null, null)
    alignment
  }

  def combineMatchingsMatrix(matchings: ImmutableMap[MatchRelationURI, Double], threshold: Double): AlignmentProcess = {
    var alignment: AlignmentProcess = new MatrixSelectionMatchingAlignment(matchings, threshold)
    alignment.align(null, null)
    alignment
  }

  def validate(reference_alignment: String)(alignment: AlignmentProcess): EvaluationResult = {
    var aparser: AlignmentParser = new AlignmentParser(0);
    var reference: Alignment = aparser.parse(new File(reference_alignment).toURI());
    var evaluator: PRecEvaluator = new PRecEvaluator(reference, alignment);
    evaluator.eval(null)

    EvaluationResult(evaluator.getPrecision(), evaluator.getRecall(), evaluator.getFmeasure())
  }

  /**
   * Function that performs an outlier detection based on Rapidminer
   *
   * @param writeFunction
   * @param readFunction
   * @param matchings
   * @return
   */
  def rapidminerTest(writeFunction: ImmutableMap[MatchRelation, ImmutableMap[String, Option[Double]]] => File)(readFunction: File => ImmutableMap[MatchRelationURI, Double])(matchings: ImmutableMap[MatchRelation, ImmutableMap[String, Option[Double]]]): ImmutableMap[MatchRelationURI, Double] = {

    val input_csv: File = writeFunction(matchings)
    //Rapidminer Handling
    RapidMiner.setExecutionMode(RapidMiner.ExecutionMode.COMMAND_LINE);
    RapidMiner.init();
    var process: RProcess = new RProcess(new File("/Users/mueller/Documents/master-thesis/RapidminerRepo/oacode_only_cluster.rmp"));

    val output_csv: File = new File("output.csv");
    process.getOperator("Input").setParameter("csv_file", input_csv.getAbsolutePath())
    process.getOperator("Output").setParameter("csv_file", output_csv.getAbsolutePath())
    process.run();

    readFunction(output_csv)
  }

  /**
   * Writes matchings to a csv file
   * @param result
   * @return
   */
  def writeCSV(prefix: String)(result: ImmutableMap[MatchRelation, ImmutableMap[String, Option[Double]]]): File = {

    // prepare 
    var i: Int = 0;
    val matcher_name_to_index = MatcherRegistry.matcher_by_name.keySet.zipWithIndex toMap

    val matcher_index_to_name = MatcherRegistry.matcher_by_name.keySet.zipWithIndex.map(tuple => tuple._2 -> tuple._1) toMap
    //Init CSV Writer

    val csv_file = new File("matchings/" + prefix + "_raw_matchings.csv")
    val writer = CSVWriter.open(csv_file)

    //print Headline
    val header: List[String] = List[String]("left", "relation", "right") ::: matcher_name_to_index.values.toList.sorted.map(A => matcher_index_to_name.get(A).get)

    //convert to java UTIL List
    writer.writeRow(header)

    for (line <- result) {
      //matcher name 
      var records = new Array[String](matcher_name_to_index.size + 3)
      //matching name
      records(0) = line._1.left;
      records(1) = line._1.relation;
      records(2) = line._1.right;
      //get rows
      for (elm <- line._2) {
        //index shift because first element is match name
        records(matcher_name_to_index(elm._1) + 3) = elm._2.get + ""
      }

      writer.writeRow(records)
    }

    writer.close

    csv_file
  }

  /**
   * Reads a csv file that contains an outlier score
   * @param file
   * @return
   */
  def readCSV(file: File): ImmutableMap[MatchRelationURI, Double] = {

    val reader = CSVReader.open(file);
    val mapped_values = reader.allWithHeaders.map(tuple =>
      {
        /*println(tuple)
        println(tuple.get("left").get)
        println(tuple.get("right").get)
        println(tuple.get("relation").get)*/

        (MatchRelationURI(new URI(tuple.get("left").get), new URI(tuple.get("right").get), tuple.get("relation").get)) -> tuple.get("outlier").get.toDouble
      }) toMap

    //normalize Values
    val finalmap = mapped_values.map(A => A._1 -> A._2 / mapped_values.values.max)
    finalmap
  }

}
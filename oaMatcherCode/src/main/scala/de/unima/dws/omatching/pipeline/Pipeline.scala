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
    
    val file_onto1: File = new File("ontos/2014/conference/cmt.owl");

    val file_onto2: File = new File("ontos/2014/conference/Conference.owl");
 
    val gs_align: String ="ontos/2014/conference/reference-alignment/cmt-conference.rdf"

    val onto1: URI = file_onto1.toURI()
    val onto2: URI = file_onto2.toURI()

    matchAll(rapidminerTest(writeCSV)(readCSV))(combineMatchingsNaive)(validate(gs_align))(onto1, onto2)
  }

  def matchAll(outlierFunction: ImmutableMap[String, Map[String, Option[Double]]] => ImmutableMap[(java.net.URI, java.net.URI, String), Double])(combinationFunction: ImmutableMap[(java.net.URI, java.net.URI, String), Double] => AlignmentProcess)(evaluationFunction: AlignmentProcess => Unit)(onto1: URI, onto2: URI): Unit = {
    MatcherRegistry.init

    val matchings = MatcherRegistry.matchAll(onto1, onto2)

    evaluationFunction(combinationFunction(outlierFunction(matchings)))
  }

  def combineMatchingsNaive(matchings: ImmutableMap[(java.net.URI, java.net.URI, String), Double]): AlignmentProcess = {
    var alignment: AlignmentProcess = new SuperNaiveOutlierMatchingAlignment(matchings)
    alignment.align(null, null)
    alignment.cut(.2)
    alignment
  }

  def validate(reference_alignment: String)(alignment: AlignmentProcess): Unit = {
    var aparser: AlignmentParser = new AlignmentParser(0);
    var reference: Alignment = aparser.parse(new File(reference_alignment).toURI());

    var p: Properties = new Properties();
    var evaluator: Evaluator = new PRecEvaluator(reference, alignment);

    println(evaluator.eval(p))
  }

  /**
   * Function that performs an outlier detection based on Rapidminer
   *
   * @param writeFunction
   * @param readFunction
   * @param matchings
   * @return
   */
  def rapidminerTest(writeFunction: ImmutableMap[String, Map[String, Option[Double]]] => File)(readFunction: File => ImmutableMap[(java.net.URI, java.net.URI, String), Double])(matchings: ImmutableMap[String, Map[String, Option[Double]]]): ImmutableMap[(java.net.URI, java.net.URI, String), Double] = {

    val input_csv: File = writeFunction(matchings)
    //Rapidminer Handling
    RapidMiner.setExecutionMode(RapidMiner.ExecutionMode.COMMAND_LINE);
    RapidMiner.init();
    var process: RProcess = new RProcess(new File("/Users/mueller/Documents/master-thesis/RapidminerRepo/oacode.rmp"));

    val output_csv: File = new File("output.csv");

    process.getOperator("Input").setParameter(CSVDataReader.PARAMETER_CSV_FILE, input_csv.getAbsolutePath())
    process.getOperator("Output").setParameter(CSVExampleSetWriter.PARAMETER_CSV_FILE, output_csv.getAbsolutePath())
    process.run();

    val reader = CSVReader.open(output_csv);

    val mapped_values = reader.allWithHeaders.map(tuple =>
      {
        val matchString: String = tuple.get("Match").get
        val uri1: String = matchString.splitAt(matchString.indexOf(">"))._1.replace("<", "")
        val rest: String = matchString.drop(matchString.indexOf(">"));
        val operator: String = rest.substring(2, 4).trim()
        val uri2: String = rest.splitAt(rest.indexOf("<") + 1)._2.replace(">", "")

        /*println(uri1)
        println(uri2)
        println(operator.trim())*/

        (new URI(uri1), new URI(uri2), operator) -> tuple.get("outlier").get.toDouble
      }) toMap

    val finalmap = mapped_values.map(A => A._1 -> A._2 / mapped_values.values.max)

    finalmap
  }

  /**
   * Writes matchings to a csv file
   * @param result
   * @return
   */
  def writeCSV(result: ImmutableMap[String, Map[String, Option[Double]]]): File = {
    // prepare 
    var i: Int = 0;
    val matcher_name_to_index = MatcherRegistry.matcher_by_name.keySet.zipWithIndex toMap

    val matcher_index_to_name = MatcherRegistry.matcher_by_name.keySet.zipWithIndex.map(tuple => tuple._2 -> tuple._1) toMap
    //Init CSV Writer

    val csv_file = new File("raw_matchings.csv")
    val writer = CSVWriter.open(csv_file)

    //print Headline
    val header: List[String] = List[String]("Match") ::: matcher_name_to_index.values.toList.sorted.map(A => matcher_index_to_name.get(A).get)

    //convert to java UTIL List
    writer.writeRow(header)

    for (line <- result) {
      //matcher name 
      var records = new Array[String](matcher_name_to_index.size + 1)
      //matching name
      records(0) = line._1;
      //get rows
      for (elm <- line._2) {
        //index shift because first element is match name
        records(matcher_name_to_index(elm._1) + 1) = elm._2.get + ""
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
  def readCSV(file: File): ImmutableMap[(java.net.URI, java.net.URI, String), Double] = {

    val reader = CSVReader.open(file);
    val mapped_values = reader.allWithHeaders.map(tuple =>
      {
        val matchString: String = tuple.get("Match").get
        val uri1: String = matchString.splitAt(matchString.indexOf(">"))._1.replace("<", "")
        val rest: String = matchString.drop(matchString.indexOf(">"));
        val operator: String = rest.substring(2, 4).trim()
        val uri2: String = rest.splitAt(rest.indexOf("<") + 1)._2.replace(">", "")

        println(uri1)
        println(uri2)
        println(operator.trim())

        (new URI(uri1), new URI(uri2), operator) -> tuple.get("outlier").get.toDouble
      }) toMap

    val finalmap = mapped_values.map(A => A._1 -> A._2 / mapped_values.values.max)
    finalmap
  }

}
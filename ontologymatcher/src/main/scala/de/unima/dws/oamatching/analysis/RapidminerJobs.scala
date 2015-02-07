package de.unima.dws.oamatching.analysis

import java.io.File

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import de.unima.dws.oamatching.alex.XMLTest
import de.unima.dws.oamatching.core.MatchRelation
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.FeatureVector
import scala.collection.immutable.{Map, Iterable}

import com.rapidminer.{Process => RProcess, RapidMiner}
;


/**
 * Scala Object containing all Rapidminer Jobs used in the pipeline
 * Created by mueller on 23/01/15.
 */
object RapidminerJobs {

  RapidMiner.setExecutionMode(RapidMiner.ExecutionMode.COMMAND_LINE)
  RapidMiner.init()

  def rapidminerOutlierDetection(csv_prefix: String): (FeatureVector) => Map[MatchRelation, Double] = rapidminerOutlier(writeCSV(csv_prefix))(readCSV) _

  def rapidminerOutlierDetectionNormalized(csv_prefix: String): (FeatureVector) => Map[MatchRelation, Double] = normalizedRapidminerOutlier(normalize)(writeCSV(csv_prefix))(readCSV) _


  /**
   * Function that performs an outlier detection based on Rapidminer
   *
   * @param writeFunction
   * @param readFunction
   * @param matchings
   * @return
   */
  def rapidminerOutlier(writeFunction: FeatureVector => File)(readFunction: File => Map[MatchRelation, Double])(matchings: FeatureVector): Map[MatchRelation, Double] = {


    val input_csv: File = writeFunction(matchings)
    //Rapidminer Handling
    val output_csv: File = new File("output.csv");

    //dynamic parameter selection
    val process_file = XMLTest.transformXMLProcess("/Users/mueller/Documents/master-thesis/RapidminerRepo/oacode_only_cluster_2.rmp", Option.apply(matchings))
    val file = new File(process_file);

    var process: RProcess = new RProcess(file);

    process.getOperator("ReadVector").setParameter("csv_file", input_csv.getAbsolutePath)
    process.getOperator("Output").setParameter("csv_file", output_csv.getAbsolutePath)

    process.run()

    readFunction(output_csv)
  }

  def normalizedRapidminerOutlier(normalizeFct: (Double,Map[MatchRelation, Double]) =>  Map[MatchRelation, Double])(writeFunction: FeatureVector => File)(readFunction: File => Map[MatchRelation, Double])(matchings: FeatureVector): Map[MatchRelation, Double] = {

    val no_of_features = matchings.matcher_name_to_index.size
    val input_csv: File = writeFunction(matchings)
    //Rapidminer Handling
    val output_csv: File = new File("output.csv");

    //dynamic parameter selection
    val process_file = XMLTest.transformXMLProcess("/Users/mueller/Documents/master-thesis/RapidminerRepo/oacode_lof_new.rmp", Option.apply(matchings))
    val file = new File(process_file);

    var process: RProcess = new RProcess(file);

    process.getOperator("ReadVector").setParameter("csv_file", input_csv.getAbsolutePath)
    process.getOperator("Output").setParameter("csv_file", output_csv.getAbsolutePath)

    process.run()
    val norm_value = Math.sqrt(no_of_features.toDouble * 4)
    val test = normalizeFct(norm_value,readFunction(output_csv))

    test.foreach(value =>{

    if(value._2>0.05) println(value) })
    test
  }



  def normalize( norm_value:Double,matchings: Map[MatchRelation, Double]):Map[MatchRelation, Double] ={
    matchings.map{case(relation,value)=> (relation, (value/norm_value))}
  }


  /**
   * Writes matchings to a csv file
   * @param result
   * @return
   */
  def writeCSV(prefix: String)(result: FeatureVector): File = {

    // prepare
    val matcher_name_to_index = result.matcher_name_to_index

    val matcher_index_to_name = result.matcher_index_to_name
    //Init CSV Writer

    val csv_file = new File("matchings/" + prefix + "_raw_matchings.csv")
    val writer = CSVWriter.open(csv_file)

    //print Headline
    val header: List[String] = List[String]("left", "relation", "right", "owl_type") ::: matcher_name_to_index.values.toList.sorted.map(A => matcher_index_to_name.get(A).get)

    //convert to java UTIL List
    writer.writeRow(header)

    for (line <- result.transposed_vector) {
      //matcher name
      var records = new Array[String](matcher_name_to_index.size + 4)
      //matching name
      records(0) = line._1.left
      records(1) = line._1.relation
      records(2) = line._1.right
      records(3) = line._1.owl_type
      //get rows
      for (elm <- line._2) {
        //index shift because first element is match name
        records(matcher_name_to_index(elm._1) + 4) = elm._2 + ""
      }

      writer.writeRow(records)
    }

    writer.close()

    csv_file
  }

  /**
   * Reads a csv file that contains an outlier scorsbe
   * @param file
   * @return
   */
  def readCSV(file: File): Map[MatchRelation, Double] = {

    val reader = CSVReader.open(file)
    val mapped_values = reader.allWithHeaders.map(tuple => {

      MatchRelation(left = tuple.get("left").get, relation = tuple.get("relation").get, right = tuple.get("right").get, owl_type = tuple.get("owl_type").get) -> tuple.get("outlier").get.toDouble
    }) toMap

    //normalize Values
    val finalmap = mapped_values.map(A => A._1 -> A._2 )
    finalmap
  }

}

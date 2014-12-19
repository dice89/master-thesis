package de.unima.dws.omatching.outlierdetection

import de.unima.dws.omatching.matcher.MatchRelationURI
import de.unima.dws.omatching.matcher.MatchRelation
import java.io.File
import de.unima.dws.omatching.pipeline.MatcherRegistry
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter
import java.net.URI
import com.rapidminer.{ Process => RProcess }
import com.rapidminer.RapidMiner

object RapidminerBasedOutlierDetection {
   RapidMiner.setExecutionMode(RapidMiner.ExecutionMode.COMMAND_LINE);
  RapidMiner.init();
  
  
  def rapidminerOutlierReadWrite(csv_prefix:String) = {
    rapidminerOutlier(writeCSV(csv_prefix)) (readCSV)_
  }
  
  /**
   * Function that performs an outlier detection based on Rapidminer
   *
   * @param writeFunction
   * @param readFunction
   * @param matchings
   * @return
   */
  def rapidminerOutlier(writeFunction: Map[MatchRelation, Map[String, Option[Double]]] => File)(readFunction: File => Map[MatchRelationURI, Double])(matchings: Map[MatchRelation, Map[String, Option[Double]]]): Map[MatchRelationURI, Double] = {


    val input_csv: File = writeFunction(matchings)
    //Rapidminer Handling

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
  def writeCSV(prefix: String)(result: Map[MatchRelation, Map[String, Option[Double]]]): File = {

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
  def readCSV(file: File): Map[MatchRelationURI, Double] = {

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
package de.unima.dws.oamatching.analysis

import java.io.File

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import de.unima.dws.oamatching.alex.XMLTest
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.MatchRelation
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.FeatureVector
import scala.Predef
import scala.collection.immutable.{Map, Iterable}

import com.rapidminer.{Process => RProcess, RapidMiner}

import scala.io.Source
;


/**
 * Scala Object containing all Rapidminer Jobs used in the pipeline
 * Created by mueller on 23/01/15.
 */

case class SeparatedResults(class_matchings:(Int, Map[String, (Double, Double)],Map[MatchRelation, Double]),dp_matchings:(Int, Map[String, (Double, Double)],Map[MatchRelation, Double]), op_matchings:(Int, Map[String, (Double, Double)],Map[MatchRelation, Double]))
object RapidminerJobs {

  def init()= {
    RapidMiner.setExecutionMode(RapidMiner.ExecutionMode.COMMAND_LINE)
    RapidMiner.init()
  }

  def quit() ={
    RapidMiner.quit(RapidMiner.ExitMode.NORMAL)
  }

  def rapidminerOutlierDetection(rapidminer_file:String, base_dir:String) (csv_prefix: String, featureVector: FeatureVector): (Int, Map[String, (Double, Double)],Map[MatchRelation, Double]) = rapidminerOutlier(writeCSV(csv_prefix,base_dir))(readCSV)(rapidminer_file,base_dir)(featureVector)

  def rapidminerOutlierDetectionExperiments(run_number:Int,rapidminer_file:String, matching_file:File,parameters: Map[String, Map[String, Double]],pre_pro_key:String, process_type:String):(Int, Map[String, (Double, Double)],Map[MatchRelation, Double]) = rapidminerOutlierFromExistentFile(readCSV)(rapidminer_file)(run_number,matching_file,parameters,pre_pro_key:String, process_type:String)

  def rapidminerOutlierDetectionExperimentsSeparated(run_number:Int,rapidminer_file:String, matching_file:File,parameters: Map[String, Map[String, Double]],pre_pro_key:String, process_type:String): SeparatedResults = rapidminerOutlierFromExistentFileSeparated(readCSV)(rapidminer_file)(run_number,matching_file,parameters,pre_pro_key:String, process_type:String)

  /**
   * Function that performs an outlier detection based on Rapidminer
   *
   * @param writeFunction
   * @param readFunction
   * @param matchings
   * @return
   */
  def rapidminerOutlier(writeFunction: FeatureVector => File)(readFunction: File => (Int, Map[String, (Double, Double)],Map[MatchRelation, Double]))(rapidminer_file:String,oa_base_dir:String)(matchings: FeatureVector): (Int, Map[String, (Double, Double)],Map[MatchRelation, Double]) = {
    val input_csv: File = writeFunction(matchings)
    //Rapidminer Handling
    val data_set_size:Int =  matchings.transposed_vector.size
    val dimensions:Int = matchings.matcher_name_to_index.size

    val output_csv: File = new File(oa_base_dir+File.separator+"output.csv");

    //dynamic parameter selection
    val file_name =System.currentTimeMillis().toString+".rmp"
    val process_file = XMLTest.transformXMLProcess(rapidminer_file, matchings.matcher_name_to_index,file_name)
    val file = new File(process_file);

    var process: RProcess = new RProcess(file);
    process.getOperator("Process").setParameter("logverbosity","error")



    process.getOperator("ReadVector").setParameter("csv_file", input_csv.getAbsolutePath)
    process.getOperator("Output").setParameter("csv_file", output_csv.getAbsolutePath)

    //quick hack to make process specific configurations
    if(rapidminer_file.contains("loop")){
      //set k to 10%
      val k_value = Math.ceil(data_set_size.toDouble*0.05).toInt


      process.getOperator("Loop").setParameter("k",k_value+"")

      val norm_factor = 0.2 *dimensions.toDouble
      process.getOperator("Loop").setParameter("normalization factor",norm_factor+"")
      println(norm_factor)
    }


    process.run()

    //trigger garbage collection
    System.gc()

    readFunction(output_csv)
  }


  def rapidminerOutlierFromExistentFile(readFunction: File => (Int, Map[String, (Double, Double)],Map[MatchRelation, Double]))(rapidminer_file:String)(run_number:Int, matching_file: File, parameters: Map[String, Map[String, Double]], pre_pro_key:String, process_type:String): (Int, Map[String, (Double, Double)],Map[MatchRelation, Double]) = {
    //get list of matchers in file
    val matching_lines = Source.fromFile(matching_file).getLines()
    val header_line =  matching_lines.next()
    val data_set_size = matching_lines.size-1; //minus 1 for header lines
    //split by comma

    //left,relation,right,owl_type, <--- filter out those fields
    val meta_data_fields:List[String] = List("left","relation","right","owl_type")
    val matcher_name_to_index: Map[String, Int] = header_line.split(",").filterNot(field => meta_data_fields.contains(field)).zipWithIndex.toMap

    val output_csv: File = new File("thesisexperiments/outliermatchings"+File.separator+process_type+s"_$run_number"+"_"+System.currentTimeMillis()+matching_file.getName);

    val file_name = pre_pro_key+process_type+System.currentTimeMillis().toString+".rmp"
    val process_file = XMLTest.transformXMLProcess(rapidminer_file, matcher_name_to_index,file_name)

    val file = new File(process_file);

    var process: RProcess = new RProcess(file);

    //process.getOperator("Process").setParameter("logverbosity","error")


    val pca_operator_name = "PCA"
    val remove_useless_name = "REMOVE_USELESS"
    val remove_correlated_name = "REMOVE_CORRELATED"

    configurePrePro(parameters, pre_pro_key, process, pca_operator_name, remove_useless_name, remove_correlated_name)


    val mining_params = parameters.get("mining").get

    if(process_type.equals("knn")){
      configure_knn(data_set_size, process, mining_params,"KNN")

    }else if(process_type.equals("cblof_regular_db")  ){

      configure_cblof_dbscan(process, mining_params, "DBSCAN", "CBLOF")

    }else if(process_type.equals("cblof_regular_x_means")  ){

      configure_cblof_xmeans(process, mining_params, "XMEANS", "CBLOF")

    }else if(process_type.equals("cblof_x_means")  ){

      configure_cblof_xmeans(process, mining_params, "XMEANS", "CBLOF")
    }else if(process_type.equals("ldcof_regular_x_means")  ){

      configure_xmeans_ldcof(process, mining_params, "XMEANS", "LDCOF")

    }else if(process_type.equals("ldcof_regular_db_scan")  ){

      configure_dbscan_ldcof(process, mining_params, "DBSCAN", "LDCOF")

    }else if(process_type.equals("lcdof_x_means")  ){
      
      configure_xmeans_ldcof(process, mining_params, "XMEANS", "LDCOF")

    }else if(process_type.equals("lof_regular")){

      configure_lof(process, mining_params, data_set_size, "LOF")

    }else if(process_type.equals("lof")  ){
      configure_lof(process, mining_params, data_set_size, "LOF")

    }else if(process_type.equals("loop")  ){
      configure_loop(data_set_size, matcher_name_to_index, process, mining_params, "LOOP")
    }
    //TODO more

    /*if(rapidminer_file.contains("loop")){
      //set k to 10%
      val k_value = Math.ceil(data_set_size.toDouble*0.05).toInt
      println(k_value)
      process.getOperator("LOOP").setParameter("k",k_value+"")

      val norm_factor = 0.2 *matcher_name_to_index.size.toDouble
      process.getOperator("LOOP").setParameter("normalization factor",norm_factor+"")
      println(norm_factor)
    }else if(rapidminer_file.contains("_lof")){
      val k_min_value = Math.ceil(data_set_size.toDouble*0.02).toInt
      val k_max_value = Math.ceil(data_set_size.toDouble*0.045).toInt

      println(k_min_value)
      println(k_max_value)
      process.getOperator("LOF").setParameter("k_min (MinPtsLB)",k_min_value+"")
      process.getOperator("LOF").setParameter("k_max (MinPtsUB)",k_max_value+"")

    }*/

    process.getOperator("ReadVector").setParameter("csv_file", matching_file.getAbsolutePath)
    process.getOperator("Output").setParameter("csv_file", output_csv.getAbsolutePath)



    try {
      process.run()
    }
    catch {
      case e:Throwable => {
        println(matching_file.getAbsolutePath)
        e.printStackTrace()
      }
    }

    //trigger garbage collection

    readFunction(output_csv)
  }

  /**
   *
   * @param readFunction
   * @param rapidminer_file
   * @param matching_file
   * @param parameters
   * @param pre_pro_key
   * @param process_type
   * @return
   */
  def rapidminerOutlierFromExistentFileSeparated(readFunction: File => (Int, Map[String, (Double, Double)],Map[MatchRelation, Double]))(rapidminer_file:String)(run_number:Int,matching_file: File,parameters: Map[String, Map[String, Double]],pre_pro_key:String, process_type:String): SeparatedResults = {
    //get list of matchers in file
    val matching_lines = Source.fromFile(matching_file).getLines()
    val header_line =  matching_lines.next()
    val data_set_size = matching_lines.size-1; //minus 1 for header lines
    //split by comma
    //get sizes by owl type
    val line_by_headers = CSVReader.open(matching_file).allWithHeaders()
    val lines_by_owl_type: Map[String, List[Predef.Map[String, String]]] = line_by_headers.groupBy(_.get("owl_type").get)
    val size_by_owl_type = lines_by_owl_type.map{case(key,list)=> key->list.size}

    //println(size_by_owl_type)
    //left,relation,right,owl_type, <--- filter out those fields
    val meta_data_fields:List[String] = List("left","relation","right","owl_type")
    val matcher_name_to_index: Map[String, Int] = header_line.split(",").filterNot(field => meta_data_fields.contains(field)).zipWithIndex.toMap


    val output_csv_classes: File = new File("thesisexperiments/outliermatchings"+File.separator+process_type+s"classes_$run_number"+"_"+System.currentTimeMillis()+matching_file.getName);
    val output_csv_dp: File = new File("thesisexperiments/outliermatchings"+File.separator+process_type+s"dp_$run_number"+"_"+System.currentTimeMillis()+matching_file.getName);
    val output_csv_op: File = new File("thesisexperiments/outliermatchings"+File.separator+process_type+s"op_$run_number"+"_"+System.currentTimeMillis()+matching_file.getName);


    val file_name = pre_pro_key+process_type+System.currentTimeMillis().toString+".rmp"

    val process_file = XMLTest.transformXMLProcess(rapidminer_file, matcher_name_to_index,file_name)

    val file = new File(process_file);

    println(process_file)
    var process: RProcess = new RProcess(file);
   // process.getOperator("Process").setParameter("logverbosity","error")

    configurePrePro(parameters, pre_pro_key, process, "PCA_C", "REMOVE_USELESS_C", "REMOVE_CORRELATED_C")
    configurePrePro(parameters, pre_pro_key, process, "PCA_DP", "REMOVE_USELESS_DP", "REMOVE_CORRELATED_DP")
    configurePrePro(parameters, pre_pro_key, process, "PCA_OP", "REMOVE_USELESS_OP", "REMOVE_CORRELATED_OP")


    val mining_params = parameters.get("mining").get

    val class_size = size_by_owl_type.get("c").getOrElse(0)
    val dp_size = size_by_owl_type.get("dp").getOrElse(0)
    val op_size = size_by_owl_type.get("op").getOrElse(0)


    if(process_type.equals("knn")){
      configure_knn(class_size, process, mining_params,"KNN_C")
      configure_knn(dp_size, process, mining_params,"KNN_DP")
      configure_knn(op_size, process, mining_params,"KNN_OP")
    }else if(process_type.equals("cblof_regular_db")  ){
      configure_cblof_dbscan(process, mining_params, "DBSCAN_C", "CBLOF_C")
      configure_cblof_dbscan(process, mining_params, "DBSCAN_DP", "CBLOF_DP")
      configure_cblof_dbscan(process, mining_params, "DBSCAN_OP", "CBLOF_OP")
    }else if(process_type.equals("cblof_regular_x_means")  ){
      configure_cblof_xmeans(process, mining_params, "XMEANS_C", "CBLOF_C")
      configure_cblof_xmeans(process, mining_params, "XMEANS_DP", "CBLOF_DP")
      configure_cblof_xmeans(process, mining_params, "XMEANS_OP", "CBLOF_OP")
    }else if(process_type.equals("cblof_x_means")  ){
      configure_cblof_xmeans(process, mining_params, "XMEANS_C", "CBLOF_C")
      configure_cblof_xmeans(process, mining_params, "XMEANS_DP", "CBLOF_DP")
      configure_cblof_xmeans(process, mining_params, "XMEANS_OP", "CBLOF_OP")
    }else if(process_type.equals("ldcof_regular_x_means")  ){
      configure_xmeans_ldcof(process, mining_params, "XMEANS_C", "LDCOF_C")
      configure_xmeans_ldcof(process, mining_params, "XMEANS_DP", "LDCOF_DP")
      configure_xmeans_ldcof(process, mining_params, "XMEANS_OP", "LDCOF_OP")
    }else if(process_type.equals("ldcof_regular_db_scan")  ){
      configure_dbscan_ldcof(process, mining_params, "DBSCAN_C", "LDCOF_C")
      configure_dbscan_ldcof(process, mining_params, "DBSCAN_DP", "LDCOF_DP")
      configure_dbscan_ldcof(process, mining_params, "DBSCAN_OP", "LDCOF_OP")
    }else if(process_type.equals("lcdof_x_means")  ){
      configure_xmeans_ldcof(process, mining_params, "XMEANS_C", "LDCOF_C")
      configure_xmeans_ldcof(process, mining_params, "XMEANS_DP", "LDCOF_DP")
      configure_xmeans_ldcof(process, mining_params, "XMEANS_OP", "LDCOF_OP")
    }else if(process_type.equals("lof_regular")){
      configure_lof(process, mining_params, class_size, "LOF_C")
      configure_lof(process, mining_params, dp_size, "LOF_DP")
      configure_lof(process, mining_params, op_size, "LOF_OP")
    }else if(process_type.equals("lof")  ){
      configure_lof(process, mining_params, class_size, "LOF_C")
      configure_lof(process, mining_params, dp_size, "LOF_DP")
      configure_lof(process, mining_params, op_size, "LOF_OP")
    }else if(process_type.equals("loop")  ){
      configure_loop(class_size, matcher_name_to_index, process, mining_params, "LOOP_C")
      configure_loop(dp_size, matcher_name_to_index, process, mining_params, "LOOP_DP")
      configure_loop(op_size, matcher_name_to_index, process, mining_params, "LOOP_OP")
    }


    process.getOperator("ReadVector").setParameter("csv_file", matching_file.getAbsolutePath)
    process.getOperator("Output_C").setParameter("csv_file", output_csv_classes.getAbsolutePath)
    process.getOperator("Output_DP").setParameter("csv_file", output_csv_dp.getAbsolutePath)


    process.getOperator("Output_OP").setParameter("csv_file", output_csv_op.getAbsolutePath)
    process.run()

    //trigger garbage collection

    SeparatedResults(readFunction(output_csv_classes), readFunction(output_csv_op),readFunction(output_csv_dp))
  }



  def normalize( norm_value:Double,matchings: Map[MatchRelation, Double]):Map[MatchRelation, Double] ={
    matchings.map{case(relation,value)=> (relation, (value/norm_value))}
  }


  /**
   * Writes matchings to a csv file
   * @param result
   * @return
   */
  def writeCSV(prefix: String, oa_base_dir:String)(result: FeatureVector): File = {

    // prepare
    val matcher_name_to_index = result.matcher_name_to_index

    val matcher_index_to_name = result.matcher_index_to_name
    //Init CSV Writer

    val csv_file = new File(oa_base_dir+File.separator+"matchings" +File.separator+ prefix + "_raw_matchings.csv")


    if(!csv_file.exists()){
     csv_file.createNewFile()
    }
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
  def readCSV(file: File): (Int, Map[String, (Double, Double)],Map[MatchRelation, Double]) = {
    println("Read File " + file.getName)


    // get dim names
    val metaDataFields = List("left","relation","right","owl_type","outlier")
    val reader_head = CSVReader.open(file)
    val dim_names = reader_head.all().head.filterNot(name => metaDataFields.contains(name))
    reader_head.close()


    val reader = CSVReader.open(file)
    var dim_size: Int = 0
    val lines = reader.allWithHeaders
    val mapped_values = lines.map(tuple => {
      //remove non numercial fields from count
      dim_size = tuple.size -5
      //get max score by dimensions

      MatchRelation(left = tuple.get("left").get, relation = tuple.get("relation").get, right = tuple.get("right").get, owl_type = tuple.get("owl_type").get) -> tuple.get("outlier").getOrElse("0.0").toDouble
    }) toMap

    reader.close()



    //when this fails the rapidminer output is not correct
    val numeric_data: List[Map[String, Double]] = lines.map(tuple => {
      tuple.filterNot(tuple => metaDataFields.contains(tuple._1)).map(tuple => tuple._1->tuple._2.toDouble)
    })


    //get unique names

    //build map by dimension
    val values_by_dim: Map[String, List[Double]] = dim_names.map(dimension_name => dimension_name->numeric_data.map(_.get(dimension_name).get)).toMap
    //now get for each dimension the min and max value

    val max_min_by_dim: Map[String, (Double, Double)] = values_by_dim.map{case(name,values)=> {

      val max: Double =if(values.size > 0) {
        values.maxBy(value=> value)
      }else {
        0.0
      }

      val min: Double =if(values.size > 0) {
        values.minBy(value=>value)
      }else {
        0.0
      }

      name->(max,min)
    }}

    reader.close()

    if(Config.loaded_config.getBoolean("rapidminerconfig.cleanup")){
      file.delete()
    }

    println("parsed dimension" +dim_size);
    //normalize Values
    val finalmap = mapped_values.map(A => A._1 -> A._2 )
    (dim_size,max_min_by_dim,finalmap)
  }

  /*########################################################################
                          Set Configuration
    ########################################################################*/

  def configure_knn(data_set_size: Int, process: RProcess, mining_params: Map[String, Double], knn_name:String) {
    val k_value = Math.ceil(data_set_size.toDouble * mining_params.get("k").get).toInt
    process.getOperator(knn_name).setParameter("k", k_value.toString)
  }

  def configure_loop(data_set_size: Int, matcher_name_to_index: Map[String, Int], process: RProcess, mining_params: Map[String, Double], LOOP_NAME: String): Double = {
    val k_factor = mining_params.get("k").get
    val norm_factor = mining_params.get("norm_factor").get

    val k_value = Math.ceil(data_set_size.toDouble * k_factor).toInt

    process.getOperator(LOOP_NAME).setParameter("k", k_value + "")

    val norm_value = norm_factor * matcher_name_to_index.size.toDouble
    process.getOperator(LOOP_NAME).setParameter("normalization factor", norm_value + "")
    norm_factor
  }

  def configure_lof(process: RProcess, mining_params: Map[String, Double], data_size: Int, lof_name: String) {
    val k_min_factor = mining_params.get("kmin").get
    val k_max_factor = mining_params.get("kmax").get

    val k_min_value = Math.ceil(data_size.toDouble * k_min_factor).toInt
    val k_max_value = Math.ceil(data_size.toDouble * k_max_factor).toInt


    process.getOperator(lof_name).setParameter("k_min (MinPtsLB)", k_min_value + "")
    process.getOperator(lof_name).setParameter("k_max (MinPtsUB)", k_max_value + "")
  }

  def configure_dbscan_ldcof(process: RProcess, mining_params: Map[String, Double], db_scan_name: String, ldcof_name: String) {
    val gamma = mining_params.get("gamma").get
    val min_points = mining_params.get("minpoints").get.toInt
    val epsilon = mining_params.get("epsilon").get

    process.getOperator(db_scan_name).setParameter("epsilon", epsilon.toString)
    process.getOperator(db_scan_name).setParameter("min_points", min_points.toString)

    process.getOperator(ldcof_name).setParameter("gamma", gamma.toString)
  }

  def configure_xmeans_ldcof(process: RProcess, mining_params: Map[String, Double], xmeans_name: String, ldcof_name: String) {
    val k_min_factor = mining_params.get("kmin").get.toInt
    val gamma = mining_params.get("gamma").get

    process.getOperator(xmeans_name).setParameter("k_min", k_min_factor.toString)
    process.getOperator(ldcof_name).setParameter("gamma", gamma.toString)
  }

  def configure_cblof_xmeans(process: RProcess, mining_params: Map[String, Double], xmeans_name: String, cblof_name: String) {
    val k_min = mining_params.get("kmin").get.toInt

    val alpha = mining_params.get("alpha").get
    process.getOperator(xmeans_name).setParameter("k_min", k_min.toString)
    process.getOperator(cblof_name).setParameter("alpha", alpha.toString)
  }

  def configure_cblof_dbscan(process: RProcess, mining_params: Map[String, Double], db_scan_name: String, cblof_name: String) {
    val min_points = mining_params.get("minpoints").get.toInt
    val epsilon = mining_params.get("epsilon").get
    val alpha = mining_params.get("alpha").get

    process.getOperator(db_scan_name).setParameter("epsilon", epsilon.toString)
    process.getOperator(db_scan_name).setParameter("min_points", min_points.toString)
    process.getOperator(cblof_name).setParameter("alpha", alpha.toString)
  }

  def configurePrePro(parameters: Map[String, Map[String, Double]], pre_pro_key: String, process: RProcess, pca_operator_name: String, remove_useless_name: String, remove_correlated_name: String) {


    println(pre_pro_key)
    val pre_pro_params = parameters.get(pre_pro_key).get
    // Handle pre_pro_config
    if (pre_pro_key.equals("pca_fixed")) {


      configurePCAFixed(process, pre_pro_params, pca_operator_name)

    } else if (pre_pro_key.equals("pca_variant")) {


      conifgurePCAVariant(process, pre_pro_params, pca_operator_name)
    } else if (pre_pro_key.equals("remove_corr")) {


      configureRemoveCorrelated(process, pre_pro_params, remove_useless_name, remove_correlated_name)
    }
  }

  def configurePCAFixed(process: RProcess, pre_pro_params: Map[String, Double], pca_operator_name: String) {
    process.getOperator(pca_operator_name).setParameter("number_of_components", pre_pro_params.get("number").get.toInt.toString)
  }

  def conifgurePCAVariant(process: RProcess, pre_pro_params: Map[String, Double], pca_operator_name: String) {
    process.getOperator(pca_operator_name).setParameter("variance_threshold", pre_pro_params.get("variance").get.toString)
  }

  def configureRemoveCorrelated(process: RProcess, pre_pro_params: Map[String, Double], remove_useless_name: String, remove_correlated_name: String) {
    val min_variance = pre_pro_params.get("min_variance").getOrElse(0.1)
    val max_correlation = pre_pro_params.get("corr_variance").getOrElse(0.9)

    process.getOperator(remove_useless_name).setParameter("numerical_min_deviation", min_variance + "")
    process.getOperator(remove_correlated_name).setParameter("correlation", max_correlation.toString)
  }


}

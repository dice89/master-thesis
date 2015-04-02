package de.unima.dws.oamatching.thesis

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.AggregatedEvaluationResult
import play.api.libs.json.Json


import scala.collection.immutable.Map
import scalaj.http.{Http, HttpResponse}

/**
 * Created by mueller on 04/03/15.
 */
trait ResultHandling extends LazyLogging {

  val server:String = Config.loaded_config.getString("general.serveraddress")


  def writeProbabilitiesToFile(ds_name: String, outlier_method: String, pre_pro_method: String, separated: Boolean,run_number:Integer, probabilites:Map[String,Double]):Unit = {
    val csv_file = new File(s"tmp/probabilites/probabilities$outlier_method$ds_name$pre_pro_method$separated$run_number.csv")
    val csv_result_writer = CSVWriter.open(csv_file)

    probabilites.foreach(tuple => {
      csv_result_writer.writeRow(List(tuple._1,tuple._2.toString))
    })

    csv_result_writer.close()
  }

  def createJSONResultString(ds_name: String, outlier_method: String, pre_pro_method: String, separated: Boolean, result: AggregatedEvaluationResult, parameters:Map[String, Map[String, Double]], thresholds: String): String = {

    val macro_precision = result.macro_eval_res.precision
    val macro_recall = result.macro_eval_res.recall
    val macro_f_measure = result.macro_eval_res.f1Measure

    val micro_precision = result.micro_eval_res.precision
    val micro_recall = result.micro_eval_res.recall
    val micro_f_measure = result.micro_eval_res.f1Measure

    val tp = result.micro_eval_res.truePositives
    val fp = result.micro_eval_res.falsePositives
    val fn = result.micro_eval_res.FalseNegatives



    val pre_pro_params = Json.toJson(parameters.get(pre_pro_method).get)
    val mining_params = Json.toJson(parameters.get("mining").get)

    val json_parameters = Json.toJson(parameters).toString()


    val json_string = "{\n  \"ds_name\":\""+ds_name+"\",\n  " +
      "  \"outlier_name\":\""+outlier_method+"\",\n   " +
      " \"pre_pro_name\":\""+pre_pro_method+"\",\n  " +
      "  \"separated\":"+separated+",\n  " +
      "  \"macro_result\": {\n       " +
      " \"precision\":"+macro_precision+",\n        " +
      "\"recall\":"+macro_recall+",\n" +
      "        \"fmeasure\":"+macro_f_measure+"},\n " +
      "   \"micro_result\": {\n  " +
      "      \"precision\":"+micro_precision+",\n  " +
      "      \"recall\":"+micro_recall+",\n   " +
      "     \"fmeasure\":"+micro_f_measure+"},\n " +
      "   \"tp\":"+tp+",\n " +
      "   \"fp\":"+fp+",\n " +
      "   \"fn\":"+fn+",\n  " +
      "  \"parameters\":"+json_parameters+",\n " +
      "   \"threshold\":"+thresholds+"\n}"



    json_string

  }

  def createJSONThresholdStringSeparated(separatedResult: SeparatedResult): String = {
    val c_th = separatedResult.c_threshold
    val dp_th = separatedResult.dp_threshold
    val op_th = separatedResult.dp_threshold
    "{\"classes\":"+c_th+",\"dp\":"+dp_th+",\"op\":"+op_th+"}"
  }

  def createJSONThresholdStringNonSeparated(threshold: Double): String = {
    "{\"threshold\":"+threshold+"}"
  }

  def sendResultToServer(json_string:String):Unit = {

    val proxy_host_setting = System.getenv("proxy_host")
    val proxy_port_setting = System.getenv("proxy_port")

    if(proxy_host_setting== null &&proxy_port_setting == null ){
      try{
        val response: HttpResponse[String] = Http(server).postData(json_string).header("content-type", "application/json").asString

        if(response.is4xx){
          logger.error("Problem with webservice")
        }

      }catch {
        case exception:Throwable =>{
          logger.error("Problem with webservice", exception)
        }
      }
    }else {
      try{
        val response: HttpResponse[String] = Http(server).proxy(proxy_host_setting, proxy_port_setting.toInt).postData(json_string).header("content-type", "application/json").asString

        if(response.is4xx){
          logger.error("Problem with webservice")
        }

      }catch {
        case exception:Throwable =>{
          logger.error("Problem with webservice", exception)
        }
      }
    }
  }
}

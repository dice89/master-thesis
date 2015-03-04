package de.unima.dws.oamatching.thesis
import de.unima.dws.oamatching.core.AggregatedEvaluationResult
import play.api.libs.json.Json


import scala.collection.immutable.Map
import scalaj.http.{Http, HttpResponse}

/**
 * Created by mueller on 04/03/15.
 */
trait ResultServerHandling {

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
        val response: HttpResponse[String] = Http("http://128.199.50.209:3000/api/experiments").postData(json_string).header("content-type", "application/json").asString

        if(response.is4xx){
          println(response.body)
        }

      }catch {
        case exception:Throwable =>{
          println("Result Server not online")
          exception.printStackTrace()
        }
      }
    }else {
      try{
        val response: HttpResponse[String] = Http("http://128.199.50.209:3000/api/experiments").proxy(proxy_host_setting, proxy_port_setting.toInt).postData(json_string).header("content-type", "application/json").asString

        if(response.is4xx){
          println(response.body)
        }

      }catch {
        case exception:Throwable =>{
          println("Result Server not online")
          exception.printStackTrace()
        }
      }
    }




  }
}

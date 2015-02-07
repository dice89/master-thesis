package de.unima.dws.oamatching.pipeline.Runners

import de.unima.dws.oamatching.pipeline.Runner

import scala.io.Source

/**
 * Created by mueller on 07/02/15.
 */
object RunFromFile extends App {


  def parseConfig():(String,String,Double) = {

    val paramMap: Map[String, String] = Source.fromFile("config/from_file_eval_runner.txt").getLines().map(line =>{
      val tuple = line.split("=")
      val key:String = tuple(0)
      val value:String = tuple(1)

      if(key.equals("file")){
        key->value
      }else if(key.equals("ref")){
        key->value
      }else if(key.equals("thresh")){
        key->value
      }else{
        ""->""
      }

    }).toMap
    (paramMap.get("file").get,paramMap.get("ref").get, paramMap.get("thresh").get.toDouble)
  }


  val config = parseConfig()

  Runner.runEvaluateFromRapidminerFile(config._1,config._2,config._3)

}

package de.unima.dws.oamatching.pipeline.Runners

import de.unima.dws.oamatching.pipeline.Runner
import de.unima.dws.oamatching.pipeline.util.MetaDataMgmt

import scala.io.Source

/**
 * Created by mueller on 07/02/15.
 */
object RunSinglePipeline extends App{

  def parseConfig():(String,String,String,String) = {

    val paramMap: Map[String, String] = Source.fromFile("config/single_pipeline_runner_config.txt").getLines().map(line =>{
      val tuple = line.split("=")
      val key:String = tuple(0)
      val value:String = tuple(1)

      if(key.equals("onto1")){
        key->value
      }else if(key.equals("onto2")){
        key->value
      }else if(key.equals("ref")){
        key->value
      } else if(key.equals("dsname")){
          key->value
      }else{
        ""->""
      }

    }).toMap
    (paramMap.get("onto1").get,paramMap.get("onto2").get,paramMap.get("ref").get,paramMap.get("dsname").get)
  }
  //load configurations
  val config = parseConfig()
  val pipeline_config = Runner.parseRunConfig();

  Runner.runSinglePlatform(config._1, config._2,config._3,pipeline_config)
}

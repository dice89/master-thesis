package de.unima.dws.omatching.pipeline

import com.redis.RedisClient

/**
 * 
 * Playground
 * @author mueller
 *
 */
object TestEnv {

  def main(args: Array[String]): Unit = {
    
    /*
		 val r:RedisClient = new RedisClient("localhost",6379)
		  r.set("TEST", 0.3)
		  println(r.get("TEST").get)*/
		  
		  
		  ThresholdOptimizationPlatform.setThreshold("A", "B", 0.3)
		  
		  println(ThresholdOptimizationPlatform.getThreshold("A", "B").getOrElse(0.0))
    //SimpleMeasures.computePrefixBiDirectional("Alexander_Mueler","Alexander");
 }

}
package de.unima.dws.omatching.pipeline

import de.unima.dws.oamatching.measures.SimpleMeasures
import de.unima.dws.oamatching.util.wordnet.WordNetHelper
import de.uniman.dws.oamatching.logging.ResultLogger

/**
 * 
 * Playground
 * @author mueller
 *
 */
object TestEnv {

  def main(args: Array[String]): Unit = {
    ResultLogger log("test")
    println(SimpleMeasures.computeLin("write", "paper"))
    
    println(WordNetHelper.getInstance().getWnstemmer().Stem("writes"))
    println(WordNetHelper.getInstance().getWnstemmer().Stem("paper"))
    //SimpleMeasures.computePrefixBiDirectional("Alexander_Mueler","Alexander");
  }

}
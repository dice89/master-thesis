package de.unima.dws.oamatching.config

import scala.io.Source

object Config {

  //Redis configuration
  val REDIS_HOST: String = "localhost"
  val REDIS_PORT: Int = 6379
  val REDIS_THRESHOLD_HASH_KEY = "MA_THRESHOLDS"

  val DEFAULT_MATCHER_THRESHOLD = 0.8;
  //webserbice based config
  val UMBC_STS_SERVICE_URL = "http://swoogle.umbc.edu/StsService/GetStsSim"
  val UMBC_PHRASE_SIM_SERVICE_URL = "http://swoogle.umbc.edu/SimService/GetSimilarity"
  val BIG_HUGE_THESAURUS_SERVICE_URL = "http://words.bighugelabs.com/api/2/74a732bd0c883ad86cc768493c0ccbac"
  //external models
  var WORD_2_VEC_MODEL_PATH: String = null
  var WORD_2_VEC_STEMMED_MODEL_PATH: String = null
  // outlier analysis conig
  var OA_BASE_DIR: String = null
  var OA_PROCESS:String = null

  //dataset paths
  val PATH_TO_CONFERENCE: String = "ontos/2014/conference"

  //Matcher configuration files
  val PATH_TO_SMALL_SCALE_CONFIG = "config/matcher_config_small_scale.csv"
  val PATH_TO_LARGE_SCALE_CONFIG = "config/matcher_config_large_scale.csv"

  val PATH_TO_STOP_LIST = "config/stoplist.txt"

  parseConfig()


  def parseConfig() = {

    Source.fromFile("config/pipeline_config.txt").getLines().foreach(line => {
      val tuple = line.split("=")
      val key: String = tuple(0)
      val value: String = tuple(1)
      if (key.equals("w2vmodels")) {
        WORD_2_VEC_MODEL_PATH = value + "/model_word2vec.ser"
        WORD_2_VEC_STEMMED_MODEL_PATH = value + "/model_word2vec_stemmed.ser"
      } else if (key.equals("oabasedir")) {
        OA_BASE_DIR = value;
      } else if (key.equals("oaprocess")) {
        OA_PROCESS = value;
      }
    })

  }


}
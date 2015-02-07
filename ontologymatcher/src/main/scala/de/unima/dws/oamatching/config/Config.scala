package de.unima.dws.oamatching.config

import java.io.File

import scala.io.Source

object Config {



	//Redis configuration
	val REDIS_HOST:String = "localhost"
	val REDIS_PORT:Int =6379
	val REDIS_THRESHOLD_HASH_KEY = "MA_THRESHOLDS"


	val DEFAULT_MATCHER_THRESHOLD = 0.8;
	//webserbice based config
	val UMBC_STS_SERVICE_URL = "http://swoogle.umbc.edu/StsService/GetStsSim"
	val UMBC_PHRASE_SIM_SERVICE_URL = "http://swoogle.umbc.edu/SimService/GetSimilarity"
	val BIG_HUGE_THESAURUS_SERVICE_URL ="http://words.bighugelabs.com/api/2/74a732bd0c883ad86cc768493c0ccbac"
	//external models
	var WORD_2_VEC_MODEL_PATH:String = null
	var WORD_2_VEC_STEMMED_MODEL_PATH:String = null

	//dataset paths
	val PATH_TO_CONFERENCE:String = "ontos/2014/conference"

	//Matcher configuration files
	val PATH_TO_SMALL_SCALE_CONFIG = "config/matcher_config_small_scale.csv"
	val PATH_TO_LARGE_SCALE_CONFIG = "config/matcher_config_large_scale.csv"

	val PATH_TO_STOP_LIST ="config/stoplist.txt"

	parseConfig()
	// Word Net Parameters
	/*val WNBASE_DIR:String 	= "/Users/mueller/WordNet/WordNet21"
	val WNVER:String 		=	"2.1"
	val WNDIR:String		=	WNBASE_DIR + File.separatorChar + WNVER + File.separatorChar + "dict";
	val WNIC:String			=	WNBASE_DIR + File.separatorChar + "ic-bnc-resnik-add1_2.1.dat";
	val WNTMP:String		=	WNBASE_DIR + File.separatorChar + "WNTemplate.xml";
	val WNPROP:String		=	WNBASE_DIR + File.separatorChar + "file_properties.xml";


	//Val Handling of YAM++
	val UN_KNOWN:Float		=	-Float.MaxValue
	val NOT_IS_A:Int		=	Int.MaxValue

	//how many synsets should be taken
	val SENSE_DEPTH:Int		=	10;

	//Splitter

	val MIN_LEN:Int = 3
		*/


	def parseConfig()= {

		Source.fromFile("config/pipeline_config.txt").getLines().foreach(line =>{
			val tuple = line.split("=")
			val key:String = tuple(0)
			val value:String = tuple(1)

			if(key.equals("w2vmodels")){
				println("got it" + value)
				WORD_2_VEC_MODEL_PATH = value +"/model_word2vec.ser"
				WORD_2_VEC_STEMMED_MODEL_PATH = value +"/model_word2vec_stemmed.ser"
			}
		})

	}


}
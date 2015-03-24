package de.unima.dws.oamatching.measures


import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.oamatching.analysis.SparkJobs
import de.unima.dws.oamatching.config.Config
import de.unima.dws.alex.simservice.{Config => Sim_Service_Config, SimService}
import org.apache.spark.mllib.feature.Word2VecModel
import play.api.libs.json._

import scalaj.http.{Http, HttpResponse}


/**
 * Created by mueller on 31/01/15.
 */
object SemanticMeasures extends  LazyLogging{

   //init SimService config
  Sim_Service_Config.readFromFile("config/umbc/sim_config.txt")
  var semantic_sim:SimService = null

  def word2VecSimilarityMeasure: (String, String) => Double = word2VecCosineSimilarity(SparkJobs.word_2_vec_model)_

  def word2VecSimilarityMeasureStemmed: (String, String) => Double = word2VecCosineSimilarity(SparkJobs.word_2_vec_model_stemmed)_

  def word2VecCosineSimilarity(model:Word2VecModel)(term1:String,term2:String):Double= {
    try {
      val value = SparkJobs.cousineSimilarityBetweenTerms(model,term1.trim,term2.trim)
      value
    }catch {
      case e: Exception => {
        //println(e)
        0.0
      }
    }
  }

  def umbcPhraseSim(phrase1:String,phrase2:String):Double = {

    try {
      semantic_sim.getSimilarity(phrase1, phrase2, false)
    }
    catch {
      case e:Throwable =>{
        logger.error(s"error at umbc phrase sim: $phrase1, $phrase2",e)
        0.0
      }
    }

  }


  def umbcSim(term1:String,term2:String):Double = {
    try {
      semantic_sim.getSimilarity(term1, term2, true)
    }
    catch {
      case e:Throwable =>{
        logger.error("error at umbc sim",e)
        0.0
      }
    }
  }

  def umbcPosSim(term1:String,term2:String):Double = {
    semantic_sim.getPosSimilarity(term1, term2, true)
  }


  def esaSim(term1:String,term2:String):Double = {

    try {
      val response: HttpResponse[String] = Http("http://localhost:8890/esaservice")
        .param("task", "esa")
        .param("term1", term1)
        .param("term2", term2).asString

      if (response.isNotError) {
        val intermediate = Json.parse(response.body)
        val res = intermediate.as[JsString].value.toDouble
        if (res < 0) {
          0.0
        } else {
          res
        }
      } else {
        println("No connection")
        0.0
      }
    }
    catch {
      case e:Throwable => 0.0
    }

  }


  def callSTSServiceUMBC(phrase1: String, phrase2: String): Double = {

    try {
      val response: HttpResponse[String] = Http("http://swoogle.umbc.edu/StsService/GetStsSim")
        .param("operation", "api")
        .param("phrase1", phrase1)
        .param("phrase2", phrase2).asString

      response.body.toDouble
    } catch {
      case e: Exception => {
        println("fail")
        0.0
      }
    }

  }

  /**
   * Potentially Better Results with POS Tagged words
   *
   * @param sim_type
   * @param corpus
   * @param phrase1 word with with part-of-speech-tag in the form of word_VB
   * @param phrase2 word with with part-of-speech-tag
   * @return
   */
  def callPhraseSimServiceUMBC(sim_type: String)(corpus: String)(phrase1: String, phrase2: String): Double = {


    try {
      val response: HttpResponse[String] = Http(Config.UMBC_PHRASE_SIM_SERVICE_URL)
        .param("operation", "api")
        .param("phrase1", phrase1)
        .param("phrase2", phrase2)
        .param("type", sim_type)
        .param("corpus", corpus).asString
      response.body.toDouble
    } catch {
      case e: Exception => {
        println("fal")
        0.0
      }
    }

  }

  /**
   * Potentially better results with pos-tagged words
   * @param phrase1
   * @param phrase2
   * @return
   */
  def callPhraseSimServiceUMBCRegular(phrase1: String, phrase2: String): Double = {
    try {
      val response: HttpResponse[String] = Http(Config.UMBC_PHRASE_SIM_SERVICE_URL)
        .param("operation", "api")
        .param("phrase1", phrase1)
        .param("phrase2", phrase2).asString
      val measure = response.body.toDouble
      if(measure > 0.5){
        println(phrase1 +" -- " + phrase2 +": " +measure)
      }

      if(measure.equals(Double.NegativeInfinity)){
        0.0
      }else{
        measure
      }
    } catch {
      case e: Exception => {
        println("fail")
        0.0
      }
    }

  }

  /**
   * Input words only with POS TAG
   * @return
   */
  def callPhraseServiceConceptWebbase: (String, String) => Double = callPhraseSimServiceUMBC("concept")("webbase")

  /**
   * Input words only with POS TAG
   * @return
   */
  def callPhraseServiceRelationWebbase: (String, String) => Double = callPhraseSimServiceUMBC("relation")("webbase")

  /**
   * Input words only with POS TAG
   * @return
   */
  def callPhraseServiceConceptGigaWords: (String, String) => Double = callPhraseSimServiceUMBC("concept")("gigawords")

  /**
   * Input words only with POS TAG
   * @return
   */
  def callPhraseServiceRelationGigaWords: (String, String) => Double = callPhraseSimServiceUMBC("relation")("gigawords")

  def isSynonymOf(term: String, to_check: String): Double = {
    val response: HttpResponse[String] = Http(Config.BIG_HUGE_THESAURUS_SERVICE_URL + "/" + term + "/json").asString
    if (!response.is2xx) {
      0.0
    } else {
      val json_parse = response.body.toString
      val result_json: JsValue = Json.parse(json_parse)

      //get all synonyms
      val syn_sets: Seq[JsValue] = result_json \\ "syn"

      val syn_terms = syn_sets.map(syn_set => {
        val syn_set_arr = syn_set.asInstanceOf[JsArray]
        for (syn_word <- syn_set_arr.value) yield {
          syn_word.asInstanceOf[JsString].as[String]
        }
      }).flatten.toSeq

      println(syn_terms)
      if (syn_terms.contains(to_check)) {
        1.0
      } else {
        0.0
      }
    }
  }
}

package de.unima.dws.oamatching.measures

import de.unima.dws.oamatching.analysis.SparkJobs
import de.unima.dws.oamatching.config.Config
import org.apache.spark.mllib.feature.Word2VecModel
import play.api.libs.json.{JsString, JsArray, JsValue, Json}

import scalaj.http.{Http, HttpResponse}


/**
 * Created by mueller on 31/01/15.
 */
object SemanticMeasures extends App {
  //TODO cache results (probably in redis hashmap)

  def word2VecSimilarityMeasure: (String, String) => Double = word2VecCosineSimilarity(SparkJobs.word_2_vec_model)_

  def word2VecCosineSimilarity(model:Word2VecModel)(term1:String,term2:String):Double= {
    try {
      val value = SparkJobs.cousineSimilarityBetweenTerms(SparkJobs.word_2_vec_model,term1.trim,term2.trim)
      value
    }catch {
      case e: Exception => {
        //println(e)
        0.0
      }
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

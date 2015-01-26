package de.unima.dws.oamatching.matcher.elementlevel

/**
 * Created by mueller on 22/01/15.
 */
class PreProcessedStringMatcher(override val similarity:Boolean, override val preprocess_function:(String) => String, val stringmatching_fct:(String,String) => Double ) extends  PreProcessedMatcher(similarity, preprocess_function){
  override def score(entity1: String, entity2: String): Double = {
    stringmatching_fct(entity1, entity2)
  }
}

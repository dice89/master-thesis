package de.unima.dws.oamatching.matcher.elementlevel

/**
 * Created by mueller on 22/01/15.
 */
class TokenizedStringMatcher(override val similarity:Boolean,
                             override val preprocess_function:(String) => String,
                             val tokenizer: String => List[String],
                             override val stringmatching_fct:(String,String) => Double
                             ) extends  PreProcessedStringMatcher(similarity, preprocess_function,stringmatching_fct){

  override def score(a: String, b: String): Double = {

    var summed_score:Double  =0.0
    var counter:Int = 0

    for (term_a <- tokenizer(a); term_b <- tokenizer(b)) {
      counter= counter +1

      summed_score = summed_score + stringmatching_fct(term_a, term_b)
    }

    val res:Double =    summed_score/counter

    res
  }
}
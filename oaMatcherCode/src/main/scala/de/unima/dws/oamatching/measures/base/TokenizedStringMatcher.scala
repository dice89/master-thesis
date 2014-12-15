package de.unima.dws.oamatching.measures.base

import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology

class TokenizedStringMatcher(override val preprocess: (OWLEntity, OWLOntology) => String, val tokenizer: String => List[String], val distance_measure: (String, String) => Double) extends BaseComponentMatcher(preprocess) {

  override def score(a: String, b: String): Double = {

    var summed_score:Double  =0.0
    var counter:Int = 0;

    for (term_a <- tokenizer(a); term_b <- tokenizer(b)) {
      counter= counter +1

      summed_score = summed_score + distance_measure(term_a, term_b)
    }
    
    val res:Double =    summed_score/counter

    res
  }
}
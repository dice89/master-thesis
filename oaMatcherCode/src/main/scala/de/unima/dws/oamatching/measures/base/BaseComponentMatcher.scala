package de.unima.dws.oamatching.measures.base

import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology

abstract class BaseComponentMatcher(val preprocess:(OWLEntity,OWLOntology)=>String) {
  
  
  def score ( onto_entity_a:(OWLEntity,OWLOntology), onto_entity_b:(OWLEntity,OWLOntology) ):Double = {
    score(preprocess.tupled(onto_entity_a), preprocess.tupled(onto_entity_b))
  }
  def score(a:String,b:String):Double

}
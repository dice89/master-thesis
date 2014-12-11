package de.unima.dws.oamatching.measures.base

import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology

class StringFunctionMatcher(override val preprocess:(OWLEntity,OWLOntology)=>String, val distance_measure: (String,String) => Double) extends BaseComponentMatcher(preprocess) {
	
	override def score(a:String,b:String):Double =  {
	  //println(a + "-"+ b);

	 distance_measure (a,b)
	}

}
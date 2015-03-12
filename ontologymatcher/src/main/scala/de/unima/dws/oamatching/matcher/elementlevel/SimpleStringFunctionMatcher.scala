package de.unima.dws.oamatching.matcher.elementlevel

import de.unima.dws.oamatching.core.matcher.ElementLevelMatcher
import org.semanticweb.owlapi.model.{OWLOntology, OWLClass, OWLProperty}

/**
 * Created by mueller on 21/01/15.
 */
class SimpleStringFunctionMatcher(override val  similarity:Boolean, val matching_function:(String,String) => Double)  extends ElementLevelMatcher(similarity) {


  override def alignClass(owlClass1: OWLClass, onto1:OWLOntology, owlClass2: OWLClass, onto2:OWLOntology ): Double = {
    matching_function(getLabelAndFragmentOfEntity(owlClass1,onto1),getLabelAndFragmentOfEntity(owlClass2,onto2))
  }

  override def alignObjectProperty(owlProperty1: OWLProperty, onto1:OWLOntology, owlProperty2: OWLProperty, onto2:OWLOntology ): Double = {
    matching_function(getLabelAndFragmentOfEntity(owlProperty1,onto1),getLabelAndFragmentOfEntity(owlProperty2,onto2))
  }

  override def alignDatatypeProperty(owlProperty1: OWLProperty, onto1:OWLOntology, owlProperty2: OWLProperty,onto2:OWLOntology ): Double = {
    matching_function(getLabelAndFragmentOfEntity(owlProperty1,onto1),getLabelAndFragmentOfEntity(owlProperty2,onto2))
  }


}

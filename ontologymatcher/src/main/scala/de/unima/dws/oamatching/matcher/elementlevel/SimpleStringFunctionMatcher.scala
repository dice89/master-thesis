package de.unima.dws.oamatching.matcher.elementlevel

import de.unima.dws.oamatching.core.matcher.ElementLevelMatcher
import org.semanticweb.owlapi.model.{OWLOntology, OWLClass, OWLProperty}

/**
 * Created by mueller on 21/01/15.
 */
class SimpleStringFunctionMatcher(override val  similarity:Boolean, val matching_function:(String,String) => Double)  extends ElementLevelMatcher(similarity) {


  override def alignClass(owlClass1: OWLClass, owlClass2: OWLClass): Double = {
    matching_function(owlClass1.getIRI.getShortForm,owlClass2.getIRI.getShortForm)
  }

  override def alignObjectProperty(owlProperty1: OWLProperty, owlProperty2: OWLProperty): Double = {
    matching_function(owlProperty1.getIRI.getShortForm,owlProperty2.getIRI.getShortForm)
  }

  override def alignDatatypeProperty(owlProperty1: OWLProperty, owlProperty2: OWLProperty): Double = {
    matching_function(owlProperty1.getIRI.getShortForm,owlProperty2.getIRI.getShortForm)
  }


}

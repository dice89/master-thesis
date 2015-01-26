package de.unima.dws.oamatching.matcher.elementlevel

import de.unima.dws.oamatching.core.matcher.ElementLevelMatcher
import org.semanticweb.owlapi.model.{OWLEntity, OWLClass, OWLProperty}

/**
 * Created by mueller on 22/01/15.
 */
abstract class PreProcessedMatcher(override val similarity:Boolean, val preprocess_function:(String) => String ) extends ElementLevelMatcher(similarity){


  def score(entity1:String, entity2:String):Double

   def score(owlEntity1: OWLEntity, owlEntity2: OWLEntity):Double = {

     score(preprocess_function(getFragmentOfEntity(owlEntity1)), preprocess_function(getFragmentOfEntity(owlEntity2)))
   }
  override def alignClass(owlClass1: OWLClass, owlClass2: OWLClass): Double = {
    score(owlClass1,owlClass2)
  }

  override def alignDatatypeProperty(owlProperty1: OWLProperty, owlProperty2: OWLProperty): Double =  {
    score(owlProperty1 ,owlProperty2)
  }

  override def alignObjectProperty(owlProperty1: OWLProperty, owlProperty2: OWLProperty): Double = {
    score(owlProperty1 ,owlProperty2)
  }
}

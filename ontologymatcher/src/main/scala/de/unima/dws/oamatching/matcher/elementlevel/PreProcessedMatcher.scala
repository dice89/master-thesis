package de.unima.dws.oamatching.matcher.elementlevel

import de.unima.dws.oamatching.core.matcher.ElementLevelMatcher
import org.semanticweb.owlapi.model.{OWLOntology, OWLEntity, OWLClass, OWLProperty}

/**
 * Created by mueller on 22/01/15.
 */
abstract class PreProcessedMatcher(override val similarity:Boolean,
                                   override val useLabel: Boolean,
                                   override val useFragment: Boolean,
                                   override val useComment: Boolean,
                                   val preprocess_function:(String) => String ) extends ElementLevelMatcher(similarity, useLabel,useFragment,useComment){


  def score(entity1:String, entity2:String):Double

   def score(owlEntity1: OWLEntity,onto1:OWLOntology,  owlEntity2: OWLEntity,onto2:OWLOntology ):Double = {
     score(preprocess_function(getLabelAndFragmentOfEntity(owlEntity1,onto1)), preprocess_function(getLabelAndFragmentOfEntity(owlEntity2,onto2)))
   }

  override def alignClass(owlClass1: OWLClass,onto1:OWLOntology, owlClass2: OWLClass,onto2:OWLOntology ): Double = {
    score(owlClass1,onto1,owlClass2,onto2)
  }

  override def alignDatatypeProperty(owlProperty1: OWLProperty,onto1:OWLOntology,  owlProperty2: OWLProperty,onto2:OWLOntology ): Double =  {
    score(owlProperty1,onto1,owlProperty2,onto2)
  }

  override def alignObjectProperty(owlProperty1: OWLProperty,onto1:OWLOntology,  owlProperty2: OWLProperty,onto2:OWLOntology ): Double = {
    score(owlProperty1,onto1,owlProperty2,onto2)
  }
}

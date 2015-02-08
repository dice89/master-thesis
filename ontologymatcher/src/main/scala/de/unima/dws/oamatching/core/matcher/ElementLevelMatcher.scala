package de.unima.dws.oamatching.core.matcher

import de.unima.dws.oamatching.core.{Alignment, Cell}
import org.semanticweb.owlapi.model.{OWLClass, OWLEntity, OWLOntology, OWLProperty}

import scala.collection.convert.Wrappers.JSetWrapper

/**
 * Created by mueller on 21/01/15.
 */
abstract class ElementLevelMatcher(val similarity:Boolean) extends Matcher {
  /**
   * Implements element-wise ontology matcher
   * @param onto1
   * @param onto2
   * @param threshold
   * @return
   */
  override def align( onto1:OWLOntology,  onto2:OWLOntology,threshold:Double) :Alignment = {

    val entities1:Set[OWLEntity] = JSetWrapper(onto1.getSignature).toSet
    val entities2:Set[OWLEntity] = JSetWrapper(onto2.getSignature).toSet

    val alignment:Alignment =  new Alignment(null,null)

/*
    entities1.par.foreach(entity1 => {
      entities2.par.foreach(entity2=> {
        //check for classes
        if(entity1.isOWLClass && entity2.isOWLClass){
          val similarity: Double = getSimilarity(alignClass(entity1.asOWLClass(), entity2.asOWLClass()))

          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_CLASS))
          }


        }

        //align Datatype Properties
        if(entity1.isOWLDataProperty && entity2.isOWLDataProperty){
          val similarity: Double = getSimilarity(alignDatatypeProperty(entity1.asOWLDataProperty(),entity2.asOWLDataProperty()))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=",Cell.TYPE_DT_PROPERTY))
          }
        }

        //align Object Properties
        if(entity1.isOWLObjectProperty && entity2.isOWLObjectProperty) {
          val similarity = getSimilarity(alignObjectProperty(entity1.asOWLObjectProperty(), entity2.asOWLObjectProperty()))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_OBJECT_PROPERTY))
          }
        }
      })

    })*/
    //start producing alignments time complexity is O(n^2)
    for(entity1 <- entities1;
        entity2 <- entities2){

        //check for classes
        if(entity1.isOWLClass && entity2.isOWLClass){
          val similarity: Double = getSimilarity(alignClass(entity1.asOWLClass(), entity2.asOWLClass()))

          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_CLASS))
          }


        }

        //align Datatype Properties
        if(entity1.isOWLDataProperty && entity2.isOWLDataProperty){
          val similarity: Double = getSimilarity(alignDatatypeProperty(entity1.asOWLDataProperty(),entity2.asOWLDataProperty()))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=",Cell.TYPE_DT_PROPERTY))
          }
        }

        //align Object Properties
        if(entity1.isOWLObjectProperty && entity2.isOWLObjectProperty) {
          val similarity = getSimilarity(alignObjectProperty(entity1.asOWLObjectProperty(), entity2.asOWLObjectProperty()))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_OBJECT_PROPERTY))
          }
        }

    }

    System.gc()
    //return alignment
    alignment
  }

  def alignClass(owlClass1: OWLClass, owlClass2: OWLClass ):Double

  def alignDatatypeProperty(owlProperty1: OWLProperty, owlProperty2: OWLProperty ):Double

  def alignObjectProperty(owlProperty1: OWLProperty, owlProperty2: OWLProperty ):Double


  /**
   *Method to get a similarity value even when distance
   * @param value
   * @return
   */
  private def getSimilarity(value:Double): Double = {
    if(similarity){
      value
    }else {
      1-value
    }
  }


}

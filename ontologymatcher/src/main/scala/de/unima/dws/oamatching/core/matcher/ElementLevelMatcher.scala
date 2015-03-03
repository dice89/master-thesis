package de.unima.dws.oamatching.core.matcher

import de.unima.dws.oamatching.core.{Alignment, Cell}
import org.semanticweb.owlapi.model.{OWLClass, OWLEntity, OWLOntology, OWLProperty}

import scala.collection.JavaConversions._
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

    val entities1 = onto1.getSignature.toVector
    val entities2 = onto2.getSignature.toVector

    val alignment:Alignment =  new Alignment(null,null)


    var i = 0
    var n = 0
    var entity1:OWLEntity= null
    var entity2:OWLEntity= null
    while (i < entities1.size){
      n = 0
      entity1 = entities1(i)
      while(n < entities2.size){
        entity2 = entities2(n)
        if(entity1.isOWLClass && entity2.isOWLClass){
          val similarity: Double = getSimilarity(alignClass(entity1.asOWLClass(),onto1, entity2.asOWLClass(),onto2))

          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_CLASS))
          }
        }

        if(entity1.isOWLDataProperty && entity2.isOWLDataProperty){
          val similarity: Double = getSimilarity(alignDatatypeProperty(entity1.asOWLDataProperty(),onto1,entity2.asOWLDataProperty(),onto2))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=",Cell.TYPE_DT_PROPERTY))
          }
        }
        if(entity1.isOWLObjectProperty && entity2.isOWLObjectProperty) {
          val similarity = getSimilarity(alignObjectProperty(entity1.asOWLObjectProperty(),onto1, entity2.asOWLObjectProperty(),onto2))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_OBJECT_PROPERTY))
          }
        }
        n = n+1
      }
      i = i+1
    }


    /*
    val it_e_1 = entities1.iterator
    while(it_e_1.hasNext){
      val it_e2 = entities2.iterator
      val entity1 = it_e_1.next()

      while(it_e2.hasNext){
          val entity2 = it_e2.next()
         if(entity1.isOWLClass && entity2.isOWLClass){
          val similarity: Double = getSimilarity(alignClass(entity1.asOWLClass(),onto1, entity2.asOWLClass(),onto2))

          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_CLASS))
          }
         }

        if(entity1.isOWLDataProperty && entity2.isOWLDataProperty){
          val similarity: Double = getSimilarity(alignDatatypeProperty(entity1.asOWLDataProperty(),onto1,entity2.asOWLDataProperty(),onto2))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=",Cell.TYPE_DT_PROPERTY))
          }
        }
        if(entity1.isOWLObjectProperty && entity2.isOWLObjectProperty) {
          val similarity = getSimilarity(alignObjectProperty(entity1.asOWLObjectProperty(),onto1, entity2.asOWLObjectProperty(),onto2))
          if(similarity >= threshold) {
            alignment.addToCorrespondences( new Cell(entity1.getIRI.toURI,entity2.getIRI.toURI, similarity, "=", Cell.TYPE_OBJECT_PROPERTY))
          }
        }

      }
    }*/
/*
    entities1.foreach(entity1 => {
      entities2.foreach(entity2=> {
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
    /*for(entity1 <- entities1.view;
        entity2 <- entities2.view){

        //check for classes
        if(entity1.isOWLClass && entity2.isOWLClass){
          val startime= System.currentTimeMillis()
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
*/
    System.gc()
    //return alignment
    alignment
  }

  def alignClass(owlClass1: OWLClass, onto1:OWLOntology, owlClass2: OWLClass, onto2:OWLOntology ):Double

  def alignDatatypeProperty(owlProperty1: OWLProperty, onto1:OWLOntology, owlProperty2: OWLProperty, onto2:OWLOntology ):Double

  def alignObjectProperty(owlProperty1: OWLProperty, onto1:OWLOntology, owlProperty2: OWLProperty,onto2:OWLOntology ):Double


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

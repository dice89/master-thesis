package de.unima.dws.oamatching.measures.base

import org.semanticweb.owlapi.model.OWLOntology
import scala.collection.immutable.List
import org.semanticweb.owlapi.model.OWLEntity
import com.wcohen.ss.AbstractStatisticalTokenDistance
import com.wcohen.ss.api.StringWrapper
import scala.collection.JavaConversions._
import com.wcohen.ss.BasicStringWrapperIterator

class SecondStringTokenMatcher(override val preprocess: (OWLEntity, OWLOntology) => String, val distance:AbstractStatisticalTokenDistance) extends TrainedComponentMatcher(preprocess) {

  override def init(ontology1: OWLOntology, ontology2: OWLOntology, classes1: List[OWLEntity], classes2: List[OWLEntity], props1: List[OWLEntity], props2: List[OWLEntity]) = {
	
    
    val labels: List[StringWrapper] =  classes1.map(entity => distance.prepare(preprocess (entity,ontology1))) :::
    classes2.map(entity => distance.prepare(preprocess (entity,ontology1))) :::
    props1.map(entity => distance.prepare(preprocess (entity,ontology1))) :::
    props2.map(entity => distance.prepare(preprocess (entity,ontology1)))
    
    // transform to java 
    val j_iter: java.util.Iterator[StringWrapper] = labels.iterator
    
    distance.train(new BasicStringWrapperIterator(j_iter))
  }
  
   override def score(a: String, b: String): Double = {
	  //call score on preprocessed data
     distance.score(a, b)

  }

}
package de.unima.dws.oamatching.measures.base

import org.semanticweb.owlapi.model.OWLOntology
import scala.collection.immutable.List
import org.semanticweb.owlapi.model.OWLEntity
import com.wcohen.ss.AbstractStatisticalTokenDistance
import com.wcohen.ss.api.StringWrapper
import scala.collection.JavaConversions._
import com.wcohen.ss.BasicStringWrapperIterator
import com.wcohen.ss.AbstractTokenizedStringDistance

class SecondStringTokenMatcher(override val preprocess: (OWLEntity, OWLOntology) => String, val distance: AbstractTokenizedStringDistance) extends TrainedComponentMatcher(preprocess) {

  override def init(ontology1: OWLOntology, ontology2: OWLOntology, classes1: List[OWLEntity], classes2: List[OWLEntity], props1: List[OWLEntity], props2: List[OWLEntity]) = {

    val labels: List[StringWrapper] = classes1.map(entity => distance.prepare(preprocess(entity, ontology1).toLowerCase())) :::
      classes2.map(entity => distance.prepare(preprocess(entity, ontology2).toLowerCase())) :::
      props1.map(entity => distance.prepare(preprocess(entity, ontology1).toLowerCase())) :::
      props2.map(entity => distance.prepare(preprocess(entity, ontology2).toLowerCase()))

    // transform to java 
    val j_iter: java.util.Iterator[StringWrapper] = labels.iterator
 
    distance.train(new BasicStringWrapperIterator(j_iter))
  }

  override def score(a: String, b: String): Double = {
    //call score on preprocessed data
    val score = distance.score(a.toLowerCase(), b.toLowerCase())

   /* if (score > 0.5 && a.toLowerCase().contains("name")) {
      println(a + "---" + b)
      println(distance.explainScore(distance.prepare(a.toLowerCase()), distance.prepare(b.toLowerCase())))

    }*/
    score
  }

}
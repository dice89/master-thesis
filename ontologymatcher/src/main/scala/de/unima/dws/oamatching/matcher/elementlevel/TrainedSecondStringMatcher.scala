package de.unima.dws.oamatching.matcher.elementlevel

import com.wcohen.ss.{BasicStringWrapperIterator, AbstractTokenizedStringDistance}
import com.wcohen.ss.api.StringWrapper
import de.unima.dws.oamatching.core.Alignment
import org.semanticweb.owlapi.model.{OWLEntity, OWLOntology}

import scala.collection.immutable.List
import scala.collection.JavaConversions._

/**
 *
 * Class that performs a matching based on second string techniques
 *
 */
class TrainedSecondStringMatcher(override val similarity:Boolean,
                                 override val preprocess_function:(String) => String,
                                 val distance: AbstractTokenizedStringDistance) extends TrainedMatcher(similarity,preprocess_function){


  /**
   * Performs Training for further matching
   * @param ontology1
   * @param ontology2
   * @param classes1
   * @param classes2
   * @param props1
   * @param props2
   */
  override def init(ontology1: OWLOntology, ontology2: OWLOntology, classes1: List[OWLEntity], classes2: List[OWLEntity], props1: List[OWLEntity], props2: List[OWLEntity]): Unit = {

    val labels: List[StringWrapper] = classes1.map(entity => distance.prepare( preprocess_function(getLabelAndFragmentOfEntity(entity,ontology1)).toLowerCase)) :::
      classes2.map(entity => distance.prepare( preprocess_function(getLabelAndFragmentOfEntity(entity,ontology2)).toLowerCase)) :::
      props1.map(entity => distance.prepare( preprocess_function(getLabelAndFragmentOfEntity(entity,ontology1)).toLowerCase)) :::
      props2.map(entity => distance.prepare(preprocess_function(getLabelAndFragmentOfEntity(entity,ontology2)).toLowerCase))

    // transform to java
    val j_iter: java.util.Iterator[StringWrapper] = labels.iterator

    distance.train(new BasicStringWrapperIterator(j_iter))
  }

  /**
   * Scores two entities based on their labels
   * @param entity1
   * @param entity2
   * @return
   */
  override def score(entity1: String, entity2: String): Double = {
    val score = distance.score(entity1, entity2)
    score
  }

  /**
   * Override implementation to include training phase trained element-wise element-wise ontology matcher
   * @param onto1
   * @param onto2
   * @param threshold
   * @return
   */
  override def align(onto1: OWLOntology, onto2: OWLOntology, threshold: Double): Alignment = {
    //get elements of the ontology
    val class_labels1: List[OWLEntity] = new JSetWrapper(onto1.getClassesInSignature).toList
    val class_labels2: List[OWLEntity] = new JSetWrapper(onto2.getClassesInSignature).toList
    val props_labels1: List[OWLEntity] = new JSetWrapper(onto1.getAnnotationPropertiesInSignature).toList
    val props_labels2: List[OWLEntity] = new JSetWrapper(onto2.getAnnotationPropertiesInSignature).toList


    init(onto1,onto2, class_labels1, class_labels2, props_labels1,props_labels2)

    super.align(onto1, onto2, threshold)
  }
}

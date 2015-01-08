package de.unima.dws.oamatching.measures

import fr.inrialpes.exmo.align.impl.MatrixMeasure
import de.unima.dws.oamatching.measures.base.BaseComponentMatcher
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology

class StandardMeasure(val baseMatcher: BaseComponentMatcher) extends MatrixMeasure {

  def this(similarity: Boolean, baseMatcher: BaseComponentMatcher) {
    this(baseMatcher)
    this similarity = similarity;
  }

  def measure(cl1: OWLEntity, cl2: OWLEntity): Double = {

	if (cl1.getIRI().toString().contains("Mexico")){
	  println("test")
	}

      val ontology1 = onto1.getOntology().asInstanceOf[OWLOntology]
      val ontology2 = onto2.getOntology().asInstanceOf[OWLOntology]

      val measure = baseMatcher.score((cl1, ontology1), (cl2, ontology2))

      measure
  
  }

  def classMeasure(cl1: Object, cl2: Object): Double = {
    measure(cl1.asInstanceOf[OWLEntity], cl2.asInstanceOf[OWLEntity])
  }
  def propertyMeasure(cl1: Object, cl2: Object): Double = {
    measure(cl1.asInstanceOf[OWLEntity], cl2.asInstanceOf[OWLEntity])
  }
  def individualMeasure(cl1: Object, cl2: Object): Double = {
    0.0
    // measure(cl1.asInstanceOf[OWLEntity], cl2.asInstanceOf[OWLEntity])
  }
}
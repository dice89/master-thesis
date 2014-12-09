package de.unima.dws.oamatching.measures.base

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLEntity

abstract class TrainedComponentMatcher(override val preprocess: (OWLEntity, OWLOntology) => String) extends BaseComponentMatcher(preprocess) {

  def init(ontology1: OWLOntology, ontology2: OWLOntology, classes1: List[OWLEntity], classes2: List[OWLEntity], props1: List[OWLEntity], props2: List[OWLEntity])

}
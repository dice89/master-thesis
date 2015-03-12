package de.unima.dws.oamatching.matcher.elementlevel

import org.semanticweb.owlapi.model.{OWLEntity, OWLOntology}

/**
 * Created by mueller on 22/01/15.
 */
abstract class TrainedMatcher(override val similarity:Boolean,
                              override val useLabel: Boolean,
                              override val useFragment: Boolean,
                              override val useComment: Boolean,
                     override val preprocess_function:(String) => String ) extends PreProcessedMatcher(similarity,useLabel,useFragment,useComment,preprocess_function){

  def init(ontology1: OWLOntology, ontology2: OWLOntology, classes1: List[OWLEntity], classes2: List[OWLEntity], props1: List[OWLEntity], props2: List[OWLEntity])


}

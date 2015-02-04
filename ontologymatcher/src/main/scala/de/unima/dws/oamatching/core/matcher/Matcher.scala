package de.unima.dws.oamatching.core.matcher

import de.unima.dws.oamatching.core.Alignment
import de.unima.dws.oamatching.pipeline.MatchingProblem
import org.semanticweb.owlapi.model.{OWLEntity, OWLOntology}

/**
 * Created by mueller on 22/01/15.
 */
abstract class Matcher {

  protected def align( onto1:OWLOntology,  onto2:OWLOntology,threshold:Double) :Alignment

  def align(problem:MatchingProblem, threshold:Double):Alignment = {
    align(problem.ontology1,problem.ontology2,threshold)
  }

  def getFragmentOfEntity(oWLEntity: OWLEntity):String = {
    oWLEntity.getIRI.getShortForm
  }

}
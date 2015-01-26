package de.unima.dws.oamatching.core

import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLOntologyManager, IRI, OWLOntology}

/**
 * Created by mueller on 21/01/15.
 */
object OntologyLoader {



  def load(onto_file:File):OWLOntology = {
    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()

    val onto_iri: IRI = IRI.create(onto_file)
    manager.loadOntology(onto_iri)
  }

  def load(onto_file:String):OWLOntology = {
    val f:File = new File(onto_file)
    load(f)
  }

}

package de.unima.dws.oamatching.core.ontology

import org.semanticweb.owlapi.model.IRI

import scala.collection.mutable

/**
 * Created by mueller on 14/03/15.
 */
class FastOntology(val classes:Set[IRI],
                   val properties:Set[IRI],
                   val class_child_map:Map[IRI, Set[IRI]],
                   val class_parent_map:Map[IRI, Set[IRI]],
                   val property_child_map:Map[IRI, Set[IRI]],
                   val property_parent_map:Map[IRI, Set[IRI]],
                   val class_in_domain_of_prop:mutable.Map[IRI, Set[IRI]],
                   val class_in_range_of_prop:mutable.Map[IRI, Set[IRI]],
                   val property_domain:mutable.Map[IRI, Set[IRI]],
                   val property_range:mutable.Map[IRI, Set[IRI]]) {




}

package de.unima.dws.oamatching.core

import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._

import scala.collection.JavaConversions._

/**
 * Created by mueller on 21/01/15.
 */
object OntologyLoader {

  def load(onto_file: File): OWLOntology = {
    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()

    val onto_iri: IRI = IRI.create(onto_file)
    manager.loadOntology(onto_iri)
  }

  def load(onto_file: String): OWLOntology = {
    val f: File = new File(onto_file)
    load(f)
  }

  def load_fast_ontology(onto: String) = {
    val onto_file: File = new File(onto)
    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()

    val onto_iri: IRI = IRI.create(onto_file)
    val owlOntology = manager.loadOntology(onto_iri)

    //get classes

    val classes: Set[IRI] = owlOntology.getClassesInSignature().map(owlClass => {
      owlClass.getIRI
    }).toSet

    val object_properties = owlOntology.getObjectPropertiesInSignature().map(prop => {
      prop.getIRI
    })

    val data_properties = owlOntology.getDataPropertiesInSignature().map(prop => {
      prop.getIRI
    })

    /*#################################################################################################################
                                            Class Hierachy
    ################################################################################################################*/
    val class_hierachy: (Set[(IRI, Set[IRI])], Set[(IRI, Set[IRI])]) = owlOntology.getClassesInSignature().map(owlClass => {
      /*
        A sub class of B
        here we are B, so get A's
      */
      val sub_classes = owlOntology.getSubClassAxiomsForSuperClass(owlClass).map(classAxiom => {
        if (!classAxiom.getSubClass.isAnonymous) {
          val sub_classes = classAxiom.getSubClass.asConjunctSet().map(sub_class => {
            sub_class.asOWLClass().getIRI
          }).toSet
          Option(sub_classes)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).flatten.toSet

      /*
        A sub class of B
        here we are A, so get B's
      */
      val super_classes = owlOntology.getSubClassAxiomsForSubClass(owlClass).map(classAxiom => {
        if (!classAxiom.getSuperClass.isAnonymous) {
          val sub_classes = classAxiom.getSuperClass.asConjunctSet().map(super_class => {
            super_class.asOWLClass().getIRI
          }).toSet
          Option(sub_classes)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).flatten.toSet

      ((owlClass.getIRI -> sub_classes), (owlClass.getIRI -> super_classes))
    }).toSet.unzip

    val parent_classes = class_hierachy._2.toMap
    val child_classes = class_hierachy._1.toMap


   /*#################################################################################################################
                                                 Data Properties Hierachy (consider removing elem types)
    ################################################################################################################*/
    val data_property_hierachy = owlOntology.getDataPropertiesInSignature().map(owlProp => {
      /*
        A sub class of B
        here we are B, so get A's
       */
      val subProperties = owlOntology.getDataSubPropertyAxiomsForSuperProperty(owlProp).map(axiom => {
        if (!axiom.getSubProperty.isAnonymous) {
          Option(axiom.getSubProperty.asOWLDataProperty().getIRI)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).toSet

      /*
        A sub class of B
        here we are A, so get B's
       */
      val superProperties = owlOntology.getDataSubPropertyAxiomsForSubProperty(owlProp).map(axiom => {
        if (!axiom.getSuperProperty.isAnonymous) {
          Option(axiom.getSuperProperty.asOWLDataProperty().getIRI)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).toSet
      ((owlProp.getIRI -> subProperties), (owlProp.getIRI -> superProperties))
    }).toSet.unzip
    val parent_data_props: Map[IRI, Set[IRI]] =data_property_hierachy._2.toMap
    val child_data_props: Map[IRI, Set[IRI]] =data_property_hierachy._1.toMap

    /*#################################################################################################################
                                                    Object Properties Hierachy
     ################################################################################################################*/
    val object_property_hierachy = owlOntology.getObjectPropertiesInSignature().map(owlProp => {
      /*
        A sub class of B
        here we are B, so get A's
       */
      val subProperties = owlOntology.getObjectSubPropertyAxiomsForSuperProperty(owlProp).map(axiom => {
        if (!axiom.getSubProperty.isAnonymous) {
          Option(axiom.getSubProperty.asOWLObjectProperty().getIRI)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).toSet

      /*
        A sub class of B
        here we are A, so get B's
       */
      val superProperties = owlOntology.getObjectSubPropertyAxiomsForSuperProperty(owlProp).map(axiom => {
        if (!axiom.getSuperProperty.isAnonymous) {
          Option(axiom.getSuperProperty.asOWLObjectProperty().getIRI)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).toSet
      ((owlProp.getIRI -> subProperties), (owlProp.getIRI -> superProperties))
    }).toSet.unzip

    val parent_object_props: Map[IRI, Set[IRI]] =object_property_hierachy._2.toMap
    val child_object_props: Map[IRI, Set[IRI]] =object_property_hierachy._1.toMap


    /*#################################################################################################################
                                                  Object Property domain and Range and the class usage in tit
      ################################################################################################################*/


    val object_prop_domain_range = owlOntology.getObjectPropertiesInSignature().map(owlProp => {

      val domain = owlOntology.getObjectPropertyDomainAxioms(owlProp).map(domain_axiom => {
        domain_axiom.getClassesInSignature.map(owl_class => {
          owl_class.getIRI
        }).toSet
      }).flatten.toSet

     val range =  owlOntology.getObjectPropertyRangeAxioms(owlProp).map(range_axiom => {
        range_axiom.getClassesInSignature.map(owl_class => {
          owl_class.getIRI
        }).toSet
      }).flatten.toSet

      (owlProp.getIRI->domain,owlProp.getIRI->range)
    }).toSet.unzip

    val object_prop_domain: Map[IRI, Set[IRI]] = object_prop_domain_range._1.toMap
    val object_prop_range: Map[IRI, Set[IRI]] = object_prop_domain_range._2.toMap




    //invert the whole thing get object properties by class, separated domain and range
    val class_object_prop_domain_range = classes.map(owlClass=> {
      //look through all domains and ranges to find the class in there
      val class_object_domain: Set[IRI] = object_prop_domain.map{case(owlProp,domain)=> {
        if(domain.contains(owlClass)){
          Option(owlProp)
        }else {
          Option.empty
        }
      }}.filter(_.isDefined).map(_.get).toSet

      val class_object_range: Set[IRI] = object_prop_range.map{case(owlProp,range)=> {
        if(range.contains(owlClass)){
          Option(owlProp)
        }else {
          Option.empty
        }
      }}.filter(_.isDefined).map(_.get).toSet

      (owlClass->class_object_domain, owlClass->class_object_range)
    }).toSet.unzip

    val class_object_prop_domain: Map[IRI, Set[IRI]] = class_object_prop_domain_range._1.toMap
    val class_object_prop_range: Map[IRI, Set[IRI]] = class_object_prop_domain_range._2.toMap


    /*#################################################################################################################
                                                 Classes in Data Property domain
      ################################################################################################################*/

    val data_prop_domain_range = owlOntology.getDataPropertiesInSignature().map(owlProp => {

      val domain = owlOntology.getDataPropertyDomainAxioms(owlProp).map(domain_axiom => {
        domain_axiom.getClassesInSignature.map(owl_class => {
          owl_class.getIRI
        }).toSet
      }).flatten.toSet


      val range =  owlOntology.getDataPropertyRangeAxioms(owlProp).map(range_axiom => {
        range_axiom.getClassesInSignature.map(owl_class => {
          owl_class.getIRI
        }).toSet
      }).flatten.toSet

      (owlProp.getIRI->domain,owlProp.getIRI->range)
    }).toSet.unzip

    val data_prop_domain: Map[IRI, Set[IRI]] =data_prop_domain_range._1.toMap
    val data_prop_range: Map[IRI, Set[IRI]] = data_prop_domain_range._2.toMap


    //invert the whole thing get data properties by class only for domain for data props
    val class_data_prop: Map[IRI, Set[IRI]] = classes.map(owlClass=> {
      //look through all domains to find the class in there
      val class_object_domain: Set[IRI] = data_prop_domain.map{case(owlProp,domain)=> {
        if(domain.contains(owlClass)){
          Option(owlProp)
        }else {
          Option.empty
        }
      }}.filter(_.isDefined).map(_.get).toSet

      owlClass->class_object_domain
    }).toMap

    /*#################################################################################################################
                                       Extract Fields for Classes
    ################################################################################################################*/




  }


}

package de.unima.dws.oamatching.matcher.structurallevel

import de.unima.dws.oamatching.core.{Cell, Alignment}
import de.unima.dws.oamatching.core.matcher.StructuralLevelMatcher
import org.semanticweb.owlapi.model._
import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Graphbased Matcher:
 * check all matched properties and match also domain and range
 * with half of confidence of matched property
 * Bla <-------new with c = 0.45--------> Blub
 * |                                      |
 * foo <--already matched with c = 0.9--> foo
 * |                                      |
 * v                                      v
 * test <-------new with c = 0.45--------> check
 * @author Sven Hertling (Original Author)
 * @author Alexander C. Mueller (Scala version)
 *
 */
class GraphBasedUsedPropertyMatcher extends StructuralLevelMatcher {

  /**
   * Implements Hot Match graph-based used property matcher,
   * so matches the classes by inspecting the domain and range
   * of properties already matches, consequently it's a very simply similarity propagation
   * @param onto1 ontology 1
   * @param onto2 ontology 2
   * @param initial_Alignment orignal input alignment
   * @param threshold threshold used
   * @return
   */
  override protected def align(onto1: OWLOntology, onto2: OWLOntology, initial_Alignment: Alignment, threshold: Double): Alignment = {


    val copied_alignment = new Alignment(initial_Alignment)

    def property_check(owlEntity: OWLEntity): Boolean = {
      (owlEntity.isOWLDataProperty || owlEntity.isOWLObjectProperty)
    }

    def getDomain(owl_entity: OWLEntity, onto: OWLOntology): Set[OWLClass] = {
      if (owl_entity.isOWLDataProperty) {
        onto.getDataPropertyDomainAxioms(owl_entity.asOWLDataProperty()).map(axiom => axiom.getClassesInSignature).flatten.toSet
      } else {
        onto.getObjectPropertyDomainAxioms(owl_entity.asOWLObjectProperty()).map(axiom => axiom.getClassesInSignature).flatten.toSet
      }
    }

    def getRange(owl_entity: OWLEntity, onto: OWLOntology): Set[OWLClass] = {
      if (owl_entity.isOWLDataProperty) {
        onto.getDataPropertyRangeAxioms(owl_entity.asOWLDataProperty()).map(axiom => axiom.getClassesInSignature).flatten.toSet
      } else {
        onto.getObjectPropertyRangeAxioms(owl_entity.asOWLObjectProperty()).map(axiom => axiom.getClassesInSignature).flatten.toSet
      }
    }

    def matchPairWise(owl_classes1: Set[OWLClass], owl_classes2: Set[OWLClass], measure: Double): Set[Cell] = {
      val cells: Set[Option[Cell]] = for (owl_class1 <- owl_classes1;
                                          owl_class2 <- owl_classes2) yield {
        val candidate = new Cell(owl_class1.getIRI.toString, owl_class2.getIRI.toString, measure, "=", Cell.TYPE_CLASS)
        if (initial_Alignment.correspondences.contains(candidate)) {
          Option.empty
        } else {
          Option(candidate)
        }
      }

      cells.filter(cell => cell.isDefined).map(cell => cell.get)

    }
    val additional_correspondences = for (cell <- initial_Alignment.correspondences) yield {
      val iri_1 = IRI.create(cell.entity1)
      val iri_2 = IRI.create(cell.entity2)
      val entity1 = onto1.getEntitiesInSignature(iri_1).head
      val entity2 = onto2.getEntitiesInSignature(iri_2).head
      val similarity = cell.measure / 2
      //check if match is property
      if (property_check(entity1) && property_check(entity2) && (similarity >= threshold)) {


        //get domain and range classes for the property
        //get domain
        val domain_1 = getDomain(entity1, onto1)
        val domain_2 = getDomain(entity2, onto2)
        //match classes in domain pair-wise

        val range_1 = getRange(entity1, onto1)
        val range_2 = getRange(entity2, onto2)

        Option(matchPairWise(domain_1, domain_2, similarity).toList ::: matchPairWise(domain_1, domain_2, similarity).toList)
        //now add add correspondences
      } else {
        Option.empty
      }
    }
    val new_correspondences = additional_correspondences.filter(cells => cells.isDefined).map(cells => cells.get).flatten.toSet



    copied_alignment
  }


}

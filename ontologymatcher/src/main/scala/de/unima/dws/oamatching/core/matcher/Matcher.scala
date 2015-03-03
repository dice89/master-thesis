package de.unima.dws.oamatching.core.matcher

import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.Alignment
import de.unima.dws.oamatching.pipeline.MatchingProblem
import org.semanticweb.owlapi.model.{OWLLiteral, OWLEntity, OWLOntology}
import scala.collection.JavaConversions._

/**
 * Created by mueller on 22/01/15.
 */
abstract class Matcher {

  protected def align( onto1:OWLOntology,  onto2:OWLOntology,threshold:Double) :Alignment

  def align(problem:MatchingProblem, threshold:Double):Alignment = {
    align(problem.ontology1,problem.ontology2,threshold)
  }

  def getFragmentOfEntity(oWLEntity: OWLEntity, ontology: OWLOntology):String = {


    val fragment = if (Config.USE_FRAGMENT){
      Option(oWLEntity.getIRI.getShortForm)
    }else {
      Option.empty
    }
    //get rdfs comment
    val comment: Option[String] = if (Config.USE_LABEL){
      val rdfs_comments = ontology.getAnnotationAssertionAxioms(oWLEntity.getIRI).filter(_.getProperty().isLabel)
      // ontology.getAnnotationAssertionAxioms(oWLEntity.getIRI).filter(_.getProperty.isLabel).foreach(println(_))
     if(rdfs_comments.size > 0){
        Option(rdfs_comments.head.getValue.asLiteral().get().getLiteral)
      }else {
        Option.empty
      }

    } else {
      Option.empty
    }
    val final_label = fragment.getOrElse("") + " " + comment.getOrElse("")

    final_label.trim
  }

}

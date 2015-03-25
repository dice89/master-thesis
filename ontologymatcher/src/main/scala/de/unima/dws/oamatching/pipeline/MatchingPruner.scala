package de.unima.dws.oamatching.pipeline

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.alcomox.{Settings, ExtractionProblem}
import de.unima.alcomox.mapping.{SemanticRelation, Correspondence, Mapping}
import de.unima.alcomox.ontology.IOntology
import de.unima.dws.oamatching.core._
import org.semanticweb.owlapi.model.OWLOntology
import scala.collection.JavaConversions._

/**
 * Implements some post matching pruning techniques
 * Created by mueller on 23/01/15.
 */
object MatchingPruner extends LazyLogging{
  val runtime = Runtime.getRuntime()
  import runtime.{ totalMemory, freeMemory, maxMemory }
  //Debugging settings
  Settings.BLACKBOX_REASONER = Settings.BlackBoxReasoner.PELLET;
  Settings.ONE_TO_ONE = false;
  /**
   * Simple namespace filter,only return matching which ids are start with the once mentioned in the allowedNamespaces list
   * @param matchings matchings to filter
   * @param allowedNameSpaces list of allowed OWL namespaces
   * @return
   */
  def nameSpaceFilter(matchings: Map[MatchRelation, Double], allowedNameSpaces: List[String]): Map[MatchRelation, Double] = {
    def checkIfContainsPrefix(name: String, prefixes: List[String]): Boolean = {
      val counter: Int = prefixes.map(prefix => name.startsWith(prefix)).count(is_prefix => is_prefix == true)
      counter > 0
    }
    //filter for return
    matchings.filter { case (relation, double) => {
      (checkIfContainsPrefix(relation.left, allowedNameSpaces) && checkIfContainsPrefix(relation.right, allowedNameSpaces))

    }
    }

  }

  /**
   * Simple namespace filter,only return matching which ids are start with the once mentioned in the allowedNamespaces list
   * @param vector vector to filter
   * @param allowedNameSpaces list of allowed OWL namespaces
   * @return
   */
  def featureVectorNameSpaceFilter(vector: FeatureVector, allowedNameSpaces: List[String]): FeatureVector = {
    def checkIfContainsPrefix(name: String, prefixes: List[String]): Boolean = {
      val counter: Int = prefixes.map(prefix => name.startsWith(prefix)).count(is_prefix => is_prefix == true)
      counter > 0
    }

    //filter for return
    val filtered = vector.vector.map { case (matcher, matchings) => {
      val filtered_matchings = matchings.filter { case (relation, double) => {
        (checkIfContainsPrefix(relation.left, allowedNameSpaces) && checkIfContainsPrefix(relation.right, allowedNameSpaces))
      }
      }
      (matcher, filtered_matchings)
    }
    }

    VectorUtil.createVectorFromResult(filtered, vector.data_set_name)

  }

  /**
   * Removes incoherent correspondences from the alignment based on alcomo
   * @param alignment
   * @return
   */
  def debugAlignment(alignment:Alignment):Alignment = {



    val owlTypeMap: Map[String, String] = alignment.correspondences.map(cell => {
      val key =cell.entity1+"="+cell.entity2
      val value = cell.owl_type
      key->value
    }).toMap

    val mapping =convertAlignmentToMapping(alignment)

    val ep = new ExtractionProblem(
      ExtractionProblem.ENTITIES_CONCEPTSPROPERTIES,
      ExtractionProblem.METHOD_GREEDY,
      ExtractionProblem.REASONING_EFFICIENT
    );

    // attach ontologies and mapping to the problem
    ep.bindSourceOntology(alignment.i_onto1);
    ep.bindTargetOntology(alignment.i_onto2);
    ep.bindMapping(mapping);


    // solve the problem
    val result = try {
      ep.solve();

      val extracted: Mapping = ep.getExtractedMapping()
      logger.info("Debugging completed")
      convertMappingToAlignment(extracted,owlTypeMap,alignment)

    }
    catch {
      case _:Throwable  =>{

        println("error")
        alignment
      }
    };

    println("New session, total memory = %s, max memory = %s, free memory = %s".format(totalMemory/1024, maxMemory/1024, freeMemory/1024))

    result
  }


  /**
   * Converts an OAMatch Alignment to an Alcomox mapping
   * @param alignment
   * @return
   */
  def convertAlignmentToMapping(alignment:Alignment):Mapping = {

    val correspondances = alignment.correspondences.map(cell=>{
      convertMatchingCellToCorrespondence(cell)
    })

    new Mapping(correspondances)
  }

  /**
   *
   * @param cell
   * @return
   */
  def convertMatchingCellToCorrespondence(cell:MatchingCell): Correspondence = {
    val correspondence = new Correspondence(cell.entity1,cell.entity2,new SemanticRelation(SemanticRelation.EQUIV), cell.measure)
    correspondence
  }

  /**
   *Converts an alcomo mapping to an alignment suitable for oamatch
   * @param mapping
   * @param owlTypeMap
   * @param undebugedAlignment the undebuged version of this created alignment
   * @return
   */
  def convertMappingToAlignment(mapping:Mapping,owlTypeMap: Map[String, String], undebugedAlignment:Alignment):Alignment = {

    val correspondences: List[MatchingCell] = mapping.getCorrespondences().map(correspondence => {
      convertCorrespondenceToCell(correspondence,owlTypeMap)
    }).toList

    new Alignment(undebugedAlignment.onto1,undebugedAlignment.onto2,undebugedAlignment.onto1_reference,undebugedAlignment.onto2_reference,null,null,correspondences)
  }

  /**
   *  Converts an alcomo mapping to a oamatch matching cell
   * @param correspondence
   * @param owlTypeMap
   * @return
   */
  def convertCorrespondenceToCell(correspondence: Correspondence,owlTypeMap: Map[String, String]):MatchingCell = {
    val measure = correspondence.getConfidence
    val entity1 = correspondence.getSourceEntityUri
    val entity2 = correspondence.getTargetEntityUri

    val relation = correspondence.getRelation.toString
    val owlType: String = owlTypeMap.getOrElse(entity1+"="+entity2,Cell.TYPE_UNKOWN)

    MatchingCell(entity1,entity2,measure,relation,owlType,Alignment.TYPE_NONE)
  }


}

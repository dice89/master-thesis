package de.unima.dws.oamatching.core

import java.net.URI

import de.unima.dws.oamatching.thesis.scalabilityTests.Tester
import org.semanticweb.owlapi.model.OWLOntology

import scala.collection.mutable

/**
 * Created by mueller on 21/01/15.
 */
case class MatchRelation(left: String, relation: String, right: String,owl_type:String)

/**
 *  Case class for a correspondance
 * @param entity1
 * @param entity2
 * @param measure
 * @param relation
 * @param owl_type
 */
case class MatchingCell(entity1:String,entity2:String,measure: Double,  relation:String,owl_type:String){
  def canEqual(other: Any): Boolean = other.isInstanceOf[MatchingCell]
  override def equals(other: Any): Boolean = other match {
    case that: MatchingCell =>{
      (that canEqual this) &&
        (this.entity1.equals(that.entity1) || this.entity1.equals(that.entity2)  ) &&
        (this.entity2.equals(that.entity2) || this.entity2.equals(that.entity1)  ) &&
        relation.equals(that.relation)
    }
    case _ => {
      true
    }

  }
  override def toString:String = {
    "[ entity1: " + entity1.toString + " ,entity2: " + entity2.toString +" ,relation: "+relation +" ]"
  }

  override def hashCode(): Int = {
    val state = Seq(entity1, entity2, relation)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}


class Alignment(val onto1:String, val onto2:String) {

  var correspondences: mutable.Set[MatchingCell] =new mutable.HashSet[MatchingCell]
  /**
   * Copy constructor
   * @param alignment_to_Copy Alingment to Copy
   */
  def this(alignment_to_Copy: Alignment) = {
    this(alignment_to_Copy.onto1,alignment_to_Copy.onto2)
    this.correspondences = alignment_to_Copy.correspondences.map(cell => MatchingCell(cell.entity1,cell.entity2,cell.measure,cell.relation,cell.owl_type ))
  }


  def this ( onto1:String,  onto2:String, correspondences:List[MatchingCell]){
    this(onto1,onto2)

   this.correspondences =  this.correspondences.++(correspondences)
  }

  /**
   * Constructor with URIS as strings
   * @param onto1
   * @param onto2
   * @param threshold
   * @param matchings
   */
  def this (onto1:String, onto2:String,threshold:Double, matchings:Map[MatchRelation,Double]){
    this(onto1,onto2)
     matchings.filter(tuple => tuple._2 >= threshold).foreach({
      case(matchrelation, similiarity) => {
        val test = MatchingCell(matchrelation.left,matchrelation.right,similiarity,matchrelation.relation,matchrelation.owl_type)

        this.correspondences.add(test)
        test
      }
    })

  }

  /**
   * Constructor for creation from a feature vector
   * @param onto1
   * @param onto2
   * @param matchings
   */
  def this (onto1:String, onto2:String, matchings:Map[MatchRelation,Double]){
    this(onto1,onto2)
    val corresp = matchings.foreach({
      case(matchrelation, similiarity) => {
        val test = MatchingCell(matchrelation.left,matchrelation.right,similiarity,matchrelation.relation,matchrelation.owl_type)
        this.correspondences.add(test)
        test

      }
    })

  }

  def addToCorrespondences(cell:MatchingCell): Unit = {

    correspondences.add(cell)
  }

  def addAllCorrespondeces(cells:Set[MatchingCell]): Unit = {

    correspondences =    mutable.HashSet(cells.toSeq:_*) ++correspondences

  }

  def addAllCorrespondeces(cells:mutable.Set[MatchingCell]): Unit = {
    correspondences=  cells ++ correspondences
  }

  def removeCorrespondence(cell_to_remove:MatchingCell):Unit = {
    correspondences = correspondences.filterNot(cell => {
      cell.entity1.eq(cell_to_remove.entity1) && cell.entity2.eq(cell_to_remove.entity2) && cell.relation.eq(cell_to_remove.relation)
    })
  }

  /**
   * Returns the correspondences as a MatchRelationMap for internal pipeline handling
   * @return
   */
  def asMatchRelationMap():Map[MatchRelation,Double] = correspondences.map(cell => {
    (MatchRelation(cell.entity1.toString,cell.relation,cell.entity2.toString,cell.owl_type),cell.measure)
  }).toMap

  override def toString :String = {
    this.asMatchRelationMap().toString()
  }


  def evaluate(reference:Alignment) :EvaluationResult = {

    //
    val tp =  if(reference.correspondences.size ==0 ){0} else  {

      correspondences.filter(cell => reference.correspondences.contains(cell)).size
    }
    //val tp =  correspondences.count(cell => reference.correspondences.contains(cell))
    //val tp =  correspondences.filter(cell => reference.correspondences.exists(cell2 => cell.equals(cell2))).size
    //false positives only present in this correspondance
    val fp = if(reference.correspondences.size ==0 ){correspondences.size}else  {
      correspondences.filterNot(cell => reference.correspondences.contains(cell)).size
    }
    //false negatives
    val fn =  if(reference.correspondences.size ==0 ){0}else  {
      reference.correspondences.filterNot(cell => correspondences.contains(cell)).size
    }


    val name = reference.onto1 +"-"+ reference.onto2
    EvaluationResultAggregator.createEvaluationResult(tp,fp,fn,name)

  }
}

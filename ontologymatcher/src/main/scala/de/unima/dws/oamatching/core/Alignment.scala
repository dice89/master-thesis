package de.unima.dws.oamatching.core

import java.net.URI

import org.semanticweb.owlapi.model.OWLOntology

import scala.collection.mutable

/**
 * Created by mueller on 21/01/15.
 */
case class MatchRelation(left: String, relation: String, right: String,owl_type:String)

class Alignment(val onto1:String, val onto2:String) {
  var correspondences: mutable.MutableList[Cell] = new mutable.MutableList[Cell]

  def this ( onto1:String,  onto2:String, correspondences:List[Cell]){
    this(onto1,onto2)

   this.correspondences =  this.correspondences++(correspondences)
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
    val corresp = matchings.filter(tuple => tuple._2 >= threshold).map({
      case(matchrelation, similiarity) => {
        new Cell(matchrelation.left,matchrelation.right,similiarity,matchrelation.relation,matchrelation.owl_type)
      }
    })
    this.correspondences =  this.correspondences++(corresp)
  }

  def this (onto1:String, onto2:String, matchings:Map[MatchRelation,Double]){
    this(onto1,onto2)
    val corresp = matchings.map({
      case(matchrelation, similiarity) => {
        new Cell(matchrelation.left,matchrelation.right,similiarity,matchrelation.relation,matchrelation.owl_type)
      }
    })
    this.correspondences =  this.correspondences++(corresp)
  }


  def addToCorrespondences(cell:Cell): Unit = {
    correspondences.+=(cell)
  }

  def removeCorrespondence(cell_to_remove:Cell):Unit = {
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
    //present in both sets

    //
    val tp =  correspondences.filter(cell => reference.correspondences.contains(cell)).size
    //val tp =  correspondences.count(cell => reference.correspondences.contains(cell))
    //val tp =  correspondences.filter(cell => reference.correspondences.exists(cell2 => cell.equals(cell2))).size
    //false positives only present in this correspondance
    val fp = correspondences.filterNot(cell => reference.correspondences.contains(cell)).size
    //true negatives
    val fn =  reference.correspondences.filterNot(cell => correspondences.contains(cell)).size

    EvaluationResultAggregator.createEvaluationResult(tp,fp,fn)




  }
}

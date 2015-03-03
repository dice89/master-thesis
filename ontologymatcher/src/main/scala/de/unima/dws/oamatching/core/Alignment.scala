package de.unima.dws.oamatching.core

import java.net.URI

import org.semanticweb.owlapi.model.OWLOntology

import scala.collection.mutable

/**
 * Created by mueller on 21/01/15.
 */
case class MatchRelation(left: String, relation: String, right: String,owl_type:String)

class Alignment(val onto1:String, val onto2:String) {
  val test:mutable.Set[Cell] = new mutable.HashSet[Cell]()
  val test2:mutable.TreeSet[(String,String,Double,String,String)] = new mutable.TreeSet[(String,String,Double,String,String)]()
  var correspondences: Set[Cell] =Set.empty[Cell]
  /**
   * Copy constructor
   * @param alignment_to_Copy Alingment to Copy
   */
  def this(alignment_to_Copy: Alignment) = {
    this(alignment_to_Copy.onto1,alignment_to_Copy.onto2)
    this.correspondences = alignment_to_Copy.correspondences.map(cell => new Cell(cell))
  }


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
        val test = new Cell(matchrelation.left,matchrelation.right,similiarity,matchrelation.relation,matchrelation.owl_type)


        test
      }
    })
    this.correspondences++=(corresp)
  }

  /**
   * Constructor for creation from a feature vector
   * @param onto1
   * @param onto2
   * @param matchings
   */
  def this (onto1:String, onto2:String, matchings:Map[MatchRelation,Double]){
    this(onto1,onto2)
    val corresp = matchings.map({
      case(matchrelation, similiarity) => {
        val test = new Cell(matchrelation.left,matchrelation.right,similiarity,matchrelation.relation,matchrelation.owl_type)

        test

      }
    })
    this.correspondences++=(corresp)
  }

  def addTest(cell:Cell):Unit = {
    test.add(cell)
  }


  def addToCorrespondences(cell:Cell): Unit = {

    correspondences.+=(cell)
  }

  def addAllCorrespondeces(cells:Set[Cell]): Unit = {

    correspondences=  cells ++ correspondences
  }

  def addAllCorrespondeces(cells:mutable.Set[Cell]): Unit = {
    val immutable_cells:Set[Cell] = Set(cells.toSeq:_*)
    correspondences=  immutable_cells ++ correspondences
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

    //
    val tp =  if(reference.correspondences.size ==0 ){0}else  {
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

    EvaluationResultAggregator.createEvaluationResult(tp,fp,fn)

  }
}

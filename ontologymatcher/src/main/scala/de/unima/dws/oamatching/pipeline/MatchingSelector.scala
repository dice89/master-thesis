package de.unima.dws.oamatching.pipeline

import java.net.URI
import scala.collection.mutable.{Set=>MutableSet}

import de.unima.dws.oamatching.core.MatchRelation

import scala.collection.immutable.Iterable

/**
 * Created by mueller on 23/01/15.
 */
object MatchingSelector {

  /**
   * Should be same mechanismn as in AML
   * @param raw_matchings
   * @param threshold
   * @return
   */
  def greedyRankSelector(raw_matchings: Map[MatchRelation, Double],threshold: Double):Map[MatchRelation, Double] ={

    val matchings = raw_matchings.filter(tuple => tuple._2 >=threshold)

    matchings.foreach(println _)

    val match_to_value = matchings.keySet.map(relation => ((relation.left.toString(), relation.right.toString()), matchings.get(relation).get)) toMap
    val match_to_owl_type = matchings.keySet.map(relation => ((relation.left.toString(), relation.right.toString()),relation.owl_type)) toMap

    //get keys as seperate list
    val keys = match_to_value.keySet.unzip

    //get both sets with unique keys as list
    val keyset_left: List[(String, Int)] = keys._1.toSet.toList.zipWithIndex
    val keyset_right: List[(String, Int)] = keys._2.toSet.toList.zipWithIndex

    val element_matrix = keyset_left.map(
    {
      case (left_element, index) =>
        keyset_right.map({ case (right_element, index) => match_to_value.get((left_element, right_element)) }) toArray
    }) toArray

    //get best per row
    val best_index_for_row = element_matrix.map(row =>
      row.zipWithIndex.reduceLeft((B, A) => {
        if (B._1.getOrElse(0.0) > A._1.getOrElse(0.0)) {
          B
        } else {
          A
        }
      }))

    //check if right is used twice, when yes pick the best


    val results = best_index_for_row.zipWithIndex.map({
      case (best_row_elem, index) => {
        val res: Double = best_row_elem._1.getOrElse(0.0)

        val uri_left = keyset_left(index)._1.trim()
        val uri_right = keyset_right(best_row_elem._2)._1.trim()

       val owl_type =  match_to_owl_type(uri_left,uri_right)
        (uri_left, uri_right, "=", owl_type,best_row_elem._1.getOrElse(0.0))
      }
    })
    //eliminate duplicate right elements
    val best_row_selected = results.groupBy({case(left,right,relation,owl_type,score) => right} ).map({case(right,list_of_relations) => list_of_relations.reduceLeft((A,B)=>{
      if(A._4 > B._4 ){
        A
      }else {
        B
      }
    })} )
    //select those that are over the threshold and add them to alignment
    val filtered_matchings: Map[MatchRelation, Double] =   best_row_selected.filter({case(left,right,relation,owl_type,score) => score >= threshold}).map({case(left,right,relation,owl_type,score) => MatchRelation(left,relation,right, owl_type)->score}).toMap

    filtered_matchings
  }



  def greedyRankSelectorSimple(raw_matchings: Map[MatchRelation, Double],threshold: Double):Map[MatchRelation, Double] = {
    val matchings: Map[MatchRelation, Double] = raw_matchings.filter(tuple => tuple._2 >=threshold)

    val sorted_matchings = matchings.toList.sortWith(_._2 > _._2)

    sorted_matchings.foreach(println _)

    var already_contained_left: MutableSet[String] = MutableSet[String]()
    var already_contained_right: MutableSet[String] = MutableSet[String]()

    //iterate over them

    val selected_matchings_raw: List[Option[(MatchRelation, Double)]] = for (matching <- sorted_matchings)yield{
      if((! already_contained_left.contains(matching._1.left)) && (! already_contained_right.contains(matching._1.right))){

         already_contained_left.add(matching._1.left)
          already_contained_right.add(matching._1.right)
        Option(matching)
      }else{
        Option.empty
      }
    }

    selected_matchings_raw.filter(_.isDefined).map(_.get).toMap
  }

}

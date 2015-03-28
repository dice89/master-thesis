package de.unima.dws.oamatching.pipeline

import de.unima.dws.oamatching.core.{Alignment, MatchRelation}

import scala.collection.mutable
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

/**
 * Created by mueller on 23/01/15.
 */
object MatchingSelector {


  def greedyRankSelectorSimpleDelta (fuzzy:Double) (raw_matchings: Map[MatchRelation, Double], threshold: Double): Map[MatchRelation, Double] = {
    val matchings: Map[MatchRelation, Double] = raw_matchings.filter(tuple => tuple._2 >= threshold)

    val sorted_matchings = matchings.toList.sortWith(_._2 > _._2)

    //sorted_matchings.foreach(println _)

    var already_contained_left: MutableSet[String] = MutableSet[String]()
    var already_contained_right: MutableSet[String] = MutableSet[String]()

    //iterate over them

    val selected_matchings_raw: List[Option[(MatchRelation, Double)]] = for (matching <- sorted_matchings) yield {
      if ((!already_contained_left.contains(matching._1.left)) && (!already_contained_right.contains(matching._1.right))) {

        already_contained_left.add(matching._1.left)
        already_contained_right.add(matching._1.right)
        Option(matching)
      } else {
        Option.empty
      }
    }

    selected_matchings_raw.filter(_.isDefined).map(_.get).map{case(relation,score)=> {
      val new_relation: MatchRelation =MatchRelation(relation.left,relation.relation,relation.right,relation.owl_type,Alignment.TYPE_NONE)
      (new_relation,score)
    }}.toMap
  }
  def greedyRankSelectorSimple = greedyRankSelectorSimpleDelta(1.0)_
  def greedyRankSelectorSimpleExp: (Double) => (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double] = greedyRankSelectorSimpleDelta _
  def fuzzyGreedyRankSelectorRatio: (Double) => (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double] = fuzzyGreedyRankSelector(selectFuzzySingleRatio)_
  def fuzzyGreedyRankSelectorDelta: (Double) => (Map[MatchRelation, Double], Double) => Map[MatchRelation, Double] = fuzzyGreedyRankSelector(selectFuzzySingleDelta)_



  def fuzzyGreedyRankSelector(select_fct: (Double,Double, (MatchRelation, Double)) => Option[(MatchRelation, Double)]) (fuzzy_value:Double)(raw_matchings: Map[MatchRelation, Double], threshold: Double): Map[MatchRelation, Double] = {
    val matchings: Map[MatchRelation, Double] = raw_matchings.filter(tuple => tuple._2 >= threshold)

    val sorted_matchings = matchings.toList.sortWith(_._2 > _._2)

    //sorted_matchings.foreach(println _)

    var already_contained_left: MutableSet[String] = MutableSet[String]()
    var already_contained_right: MutableSet[String] = MutableSet[String]()

    var already_contained_left_threshold: MutableMap[String, Double] = new mutable.HashMap[String, Double]()
    var already_contained_right_threshold: MutableMap[String, Double] = new mutable.HashMap[String, Double]()

    //iterate over them
    val selected_matchings_raw: List[Option[(MatchRelation, Double)]] =
      for (matching <- sorted_matchings) yield {
        //case when not present
        if ((!already_contained_left_threshold.contains(matching._1.left)) && (!already_contained_right_threshold.contains(matching._1.right))) {

          already_contained_left_threshold.put(matching._1.left, matching._2)
          already_contained_right_threshold.put(matching._1.right, matching._2)
          Option(matching)
        } else {
          //case one left is already in selected -> right not
          if (already_contained_left_threshold.contains(matching._1.left) && (!already_contained_right_threshold.contains(matching._1.right))) {
            val already_contained_sim_value: Double = already_contained_left_threshold.get(matching._1.left).getOrElse(0.0)

            select_fct(fuzzy_value,already_contained_sim_value, matching )

            //case two right is already in selected -> left not
          } else if (already_contained_right_threshold.contains(matching._1.right) && (!already_contained_left_threshold.contains(matching._1.left))) {
            val already_contained_sim_value: Double = already_contained_right_threshold.get(matching._1.right).getOrElse(0.0)

            select_fct(fuzzy_value,already_contained_sim_value, matching )

            //both are in -> return option empty
          } else {
            Option.empty
          }
        }
      }

    selected_matchings_raw.filter(_.isDefined).map(_.get).map{case(relation,score)=> {
      val new_relation: MatchRelation =MatchRelation(relation.left,relation.relation,relation.right,relation.owl_type,relation.match_type)
      (new_relation,score)
    }}.toMap
  }



  /**
   * Function to define wether or not to select an element already in the selected matchings should be considered
   * @param ratio_threshold
   * @param matching
   * @return
   */
  def selectFuzzySingleRatio(ratio_threshold: Double,already_contained_sim_value:Double, matching: (MatchRelation, Double)): Option[(MatchRelation, Double)] = {
    val ratio = already_contained_sim_value / matching._2
    // E.g left has already a relation with 1.0 similarity and now a second comes with 0.95 => ratio = 1.0/0.95= 1.0526315789
    if (ratio < ratio_threshold) {
      //take it
      Option(matching)
    } else {
      //leave it
      Option.empty
    }
  }

  def selectFuzzySingleDelta(delta_threshold: Double,already_contained_sim_value:Double, matching: (MatchRelation, Double)): Option[(MatchRelation, Double)] = {

    val delta = Math.abs(already_contained_sim_value - matching._2)
    // E.g left has already a relation with 1.0 similarity and now a second comes with 0.95 => delta = 1.0-0.95= 0.05
    if (delta <= delta_threshold) {
      //take it
      Option(matching)
    } else {
      //leave it
      Option.empty
    }
  }
}

package de.unima.dws.oamatching.pipeline

import de.unima.dws.oamatching.core.MatchRelation
import de.unima.dws.oamatching.pipeline._


/**
 * Implements some post matching pruning techniques
 * Created by mueller on 23/01/15.
 */
object MatchingPruner {

  /**
   * Simple namespace filter,only return matching which ids are start with the once mentioned in the allowedNamespaces list
   * @param matchings matchings to filter
   * @param allowedNameSpaces list of allowed OWL namespaces
   * @return
   */
  def nameSpaceFilter(matchings: Map[MatchRelation, Double], allowedNameSpaces: List[String]): Map[MatchRelation, Double] = {
    def checkIfContainsPrefix(name: String, prefixes: List[String]): Boolean = {
      val counter: Int = prefixes.map(prefix => name.startsWith(prefix)).count(is_prefix => is_prefix==true)
      counter > 0
    }
    //filter for return
    matchings.filter { case (relation, double) => {
      (checkIfContainsPrefix(relation.left, allowedNameSpaces)&&  checkIfContainsPrefix(relation.right, allowedNameSpaces))

    }
    }

  }

  /**
   * Simple namespace filter,only return matching which ids are start with the once mentioned in the allowedNamespaces list
   * @param matchings matchings to filter
   * @param allowedNameSpaces list of allowed OWL namespaces
   * @return
   */
  def featureVectorNameSpaceFilter(vector: FeatureVector, allowedNameSpaces: List[String]):FeatureVector = {
    def checkIfContainsPrefix(name: String, prefixes: List[String]): Boolean = {
      val counter: Int = prefixes.map(prefix => name.startsWith(prefix)).count(is_prefix => is_prefix==true)
      counter > 0
    }

    //filter for return
   val filtered= vector.vector.map{case(matcher,matchings)=> {
     val filtered_matchings =  matchings.filter { case (relation, double) => {
        (checkIfContainsPrefix(relation.left, allowedNameSpaces)&&  checkIfContainsPrefix(relation.right, allowedNameSpaces))
      }}
       (matcher,filtered_matchings)
     }}

    VectorUtil.createVectorFromResult(filtered,vector.data_set_name)

  }





}

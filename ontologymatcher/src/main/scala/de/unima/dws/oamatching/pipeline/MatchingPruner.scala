package de.unima.dws.oamatching.pipeline

import de.unima.dws.oamatching.core.MatchRelation

/**
 * Created by mueller on 23/01/15.
 */
object MatchingPruner {
  def nameSpaceFilter(matchings: Map[MatchRelation, Double],allowedNameSpaces: List[String]):Map[MatchRelation, Double] ={
    //TODO add namespace filtering
    matchings
  }
}

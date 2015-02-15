package de.unima.dws.oamatching.pipeline

import de.unima.dws.oamatching.core.MatchRelation

/**
 * Created by mueller on 10/02/15.
 */
object ScoreNormalizationFunctions {


  def getNormFunction(normFCT:String): (Int, Iterable[(MatchRelation, Double)]) => Iterable[(MatchRelation, Double)] = {
    normFCT match {
      case "none" => noNormalization _
      case "maxeuclidean"  =>  normalizeByMaxEuclideanDistance _
      case other =>  noNormalization _
    }
  }

  def normalizeByMaxEuclideanDistance(dimensions: Int, relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {
    val maxDistance = Math.sqrt(dimensions.toDouble * 4)

    relations.view.map { case (match_relation, distance) => {
      (match_relation, (distance / maxDistance))
    }
    }
  }

  def noNormalization(dimensions: Int, relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {
    relations
  }

}

package de.unima.dws.oamatching.pipeline

import de.unima.dws.oamatching.core.MatchRelation
import org.apache.commons.math.stat.descriptive.moment.Mean
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import org.apache.commons.math3.special.{Gamma, Erf}

/**
 * Created by mueller on 10/02/15.
 */
object ScoreNormalizationFunctions {
  val stdev_computer = new StandardDeviation()
  val mean_computer = new Mean()


  def getNormFunction(normFCT:String): (Int, Iterable[(MatchRelation, Double)]) => Iterable[(MatchRelation, Double)] = {
    normFCT match {
      case "none" => noNormalization _
      case "maxeuclidean"  =>  normalizeByMaxEuclideanDistance _
      case "gaussianscale" => normalizeByGaussianScaling _
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

  /**
   *  As Proposed by Kriegel et.al Interpreting and Unifying Outlier Scores
   * @param dimensions
   * @param relations
   * @return
   */
  def normalizeByGaussianScaling(dimensions: Int, relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {

    val values = relations.unzip._2.toArray

    val stdev = stdev_computer.evaluate(values)
    val mean = mean_computer.evaluate(values)


    relations.view.map { case (match_relation, distance) => {

      val pre_erf = (distance - mean)/(stdev*Math.sqrt(2))
      val scaled = Math.max(0,Erf.erf(pre_erf))

      (match_relation, scaled)
    }
    }
  }


  def normalizeByGammaScaling(dimensions: Int, relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {

    def cdfGamma(k:Double,theta:Double) (param:Double):Double = {
      Gamma.regularizedGammaP(k,param/theta)
    }

    val values = relations.unzip._2.toArray

    val expected_mean = mean_computer.evaluate(values)
   // val expected_mean_square = mean_computer.evaluate(values.map(Math.pow(_,2.0)))
    //val expected_mean_square = mean_computer.evaluate(values.map(Math.pow(_,2.0)))
    val expected_stdev= stdev_computer.evaluate(values)

    val theta =  (expected_stdev*expected_stdev)/expected_mean
    val k = (expected_mean*expected_mean) / (expected_stdev*expected_stdev)
    def cdfGamma_parameterized = cdfGamma(k,theta) _

    def mean_cdf = cdfGamma_parameterized(expected_mean)

    relations.view.map { case (match_relation, distance) => {

      val pre_selected = (cdfGamma_parameterized(distance)- mean_cdf )/(1-mean_cdf)
      val scaled = Math.max(0,pre_selected)

      (match_relation, scaled)
    }
    }
  }
  def normalizeByZScore(dimensions: Int, relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {
    val values = relations.unzip._2.toArray

    val stdev = stdev_computer.evaluate(values)
    val mean = mean_computer.evaluate(values)

    relations.view.map { case (match_relation, distance) => {

      val scaled = Math.max(0,(distance - mean) / (stdev))

      (match_relation, scaled)
    }
    }
  }




  def noNormalization(dimensions: Int, relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {
    relations
  }

}

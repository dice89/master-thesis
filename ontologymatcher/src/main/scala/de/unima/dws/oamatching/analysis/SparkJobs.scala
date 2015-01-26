package de.unima.dws.oamatching.analysis

import de.unima.dws.oamatching.core.MatchRelation
import de.unima.dws.oamatching.pipeline.FeatureVector
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.stat.{Statistics, MultivariateStatisticalSummary}
import org.apache.spark.{SparkContext, SparkConf}

import scala.collection.immutable.{Iterable, Map}

/**
 * Scala Object does contain all Spark Jobs used in the analysis pipeline
 * Created by mueller on 23/01/15.
 */
object SparkJobs {

  val conf = new SparkConf()
    .setAppName("Alex Master Thesis")
    //this needs to be parameterized.
    .setMaster("local[1]")
    .set("spark.executor.memory", "1g")

  val sc = new SparkContext(conf)

  /**
   * Function to remove correlated features from the feature vector with apache spark capabilites
   * @param feature_vector
   * @return
   */
  def removeCorrelatedFeatures(feature_vector:FeatureVector) = {

    val no_of_matcher:Int = feature_vector.matcher_index_to_name.size
    val initial_vector: Map[MatchRelation, Map[String, Double]] = feature_vector.transposed_vector

    //create column vectors for correlation matrix creation
    val columns: Iterable[Array[Double]] = initial_vector.map({ case (matchrelation, matchermap) => matchermap.values.toArray})


    //now create spark dataset
    val features = sc.parallelize(columns.map(column => Vectors.dense(column)).toList)

    //println some usefull statistics
    val test:MultivariateStatisticalSummary  =Statistics.colStats(features)
    println(test.variance)

    //correlate
    val correlMatrix = Statistics.corr(features)


    //based on the correlMatrix now perform
    println(correlMatrix)

    //create list of triples in the form (row_index,column_index, correlation)
    val row_column_value_correlMatrix: Array[(Int, Int, Double)] =  correlMatrix.toArray.zipWithIndex.map({case (value, index)=> {
      val column_index = (index % no_of_matcher)
      val row_index = Math.floor(index.toDouble / no_of_matcher.toDouble).toInt
      (row_index,column_index,value )
    }})


    //TODO print pair wise correlation

    //get those attributes with the a too high correlation, keep row attribute, remove column attribute
    val to_be_removed_parameters =  row_column_value_correlMatrix.filter({case (row,column,correlation) => (column > row) && correlation > 0.5}).map(triple => triple._2).toSet

    //rebuild feature vector

    println(to_be_removed_parameters)
    println("Total number of parameters to be removed" + to_be_removed_parameters.size)

    feature_vector
  }
}

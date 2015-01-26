package de.unima.dws.omatching.pipeline

import java.io.File
import java.net.URI

import de.unima.dws.omatching.matcher.{MatchRelation, SimpleIndividualMatcher}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.mllib.linalg.{Matrix, Vectors}
import org.apache.spark.mllib.stat.Statistics
import org.semanticweb.owl.align.Alignment

import scala.collection.immutable.{Iterable, Map}

/**
 * Created by mueller on 21/01/15.
 */

case class MatchingProblem(ontology1: URI, ontology2: URI, name: String)

case class MatchingEvaluationProblem(ontology1: URI, ontology2: URI, reference: Alignment, name: String)

case class FeatureVector(vector: Map[String,Map[MatchRelation,Double]], transposed_vector: Map[MatchRelation, Map[String, Double]] );

object MatchingPipelineCore{
  val conf = new SparkConf()
    .setAppName("Simple Application")
    //this needs to be parameterized.
    .setMaster("local[*]")
    .set("spark.executor.memory", "2g")

  val sc = new SparkContext(conf)

  def main(args: Array[String]): Unit = {

    val file_onto1: File = new File("ontos/2014/conference/cmt.owl");
    val file_onto2: File = new File("ontos/2014/conference/Conference.owl");

    val onto1: URI = file_onto1.toURI()
    val onto2: URI = file_onto2.toURI()

    val test_problem = MatchingProblem(onto1 ,onto2,"test");

    matchProblem(test_problem, null);

  }



  def matchProblem(problem: MatchingProblem, parameters: Map[String,Double] ): (Map[MatchRelation,Double],FeatureVector) = {
      val individual_matcher_results:FeatureVector = matchAllIndividualMatchers(problem)




    //get inverted Feature Vector for outlier and correlation analysis

    //this is a list of unique MatchingRelations over all results

    /*

    val results_per_matching= unique_matchings
      .map(match_relation => (match_relation, individual_matcher_results.vector.filter({case (name,match_relation_inner) => match_relation_inner.contains(match_relation)}).map(k) ) )

    val matching_score_per_matching = results_per_matching.map({case(match_relation, map_of_match_relation) => {
      map_of_match_relation.
    }})

    val results_per_matching = unique_elements.map(matching => matching -> results.filter(triple => triple._2._2.contains(matching)).map({ case (name, (eval, matchings)) => name -> matchings.get(matching) })).toMap

*/
    removeCorrelatedMatchers(individual_matcher_results);


     null
  }

  def matchIndividualMatcher(matcher:SimpleIndividualMatcher, problem: MatchingProblem):  Map[MatchRelation, Double] = {
    matcher.prepare(problem.ontology1,problem.ontology2)
    matcher.align(0.0)
  }

  /**
   *
   * @param problem
   * @return
   */
  def matchAllIndividualMatchers(problem:MatchingProblem):FeatureVector = {

   val vector = MatcherRegistry.matcher_by_name.map({case (name,matcher) => {
   (name,matchIndividualMatcher(matcher, problem))}}) toMap

    val unique_matchings: Iterable[MatchRelation] = vector.map({ case (name,  matchings) => matchings.keySet }).flatten

    val vector_per_matchings: Map[MatchRelation, Map[String, Double]] =unique_matchings.map(matching => (matching, vector.filter(tuple => tuple._2.contains(matching)).map(tuple => (tuple._1, tuple._2.get(matching).getOrElse(0.0))))).toMap;


    FeatureVector(vector,vector_per_matchings)
  }



  def removeCorrelatedMatchers(feature_vector:FeatureVector) = {

    val features = sc.parallelize(feature_vector.vector.values.map(matcher_result => Vectors.dense(matcher_result.values toArray)).toList)

    val correlMatrix: Matrix = Statistics.corr(features, "pearson")
    //TODO implement pearson correlation


    println(correlMatrix);
    feature_vector
  }

  //TODO def matchStructuralMatcher()


  def combineFeatureVectors(feature_vector1:FeatureVector, feature_vector2: FeatureVector):FeatureVector = {

    //FeatureVector(feature_vector1.vector.++:(feature_vector2.vector))
    null
  }

  def performMatcherAggregation(feature_vector: FeatureVector):Map[MatchRelation,String] = {

    null
  }




}

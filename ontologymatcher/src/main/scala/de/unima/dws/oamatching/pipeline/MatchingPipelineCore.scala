package de.unima.dws.oamatching.pipeline

import java.io.File

import de.unima.dws.oamatching.analysis.{SparkJobs, RapidminerJobs}
import de.unima.dws.oamatching.core.matcher.{StructuralLevelMatcher, Matcher}
import de.unima.dws.oamatching.core.{MatchRelation, Alignment, AlignmentParser, OntologyLoader}
import de.unima.dws.oamatching.matcher.MatcherRegistry
import org.semanticweb.owlapi.model.OWLOntology

import scala.collection.immutable.Map
import scala.collection.parallel.immutable.ParMap


case class MatchingProblem(ontology1: OWLOntology, ontology2: OWLOntology, name: String)

case class MatchingEvaluationProblem(ontology1: OWLOntology, ontology2: OWLOntology, reference: Alignment, name: String)

/**
 * Core Single to implement matching of two ontologies
 * Created by mueller on 21/01/15.
 */
object MatchingPipelineCore{

  /**
   * To execute matching process
   * @param problem
   * @param parameters
   * @return
   */
  def matchProblem(problem: MatchingProblem, parameters: Map[String,Double] ): (Alignment,FeatureVector) = {

    val onto1_namespace = problem.ontology1.getOntologyID.getOntologyIRI.get().toString
    val onto2_namespace = problem.ontology2.getOntologyID.getOntologyIRI.get().toString
    println(onto1_namespace)
    println(onto2_namespace)
    val allowed_namespaces = List(onto1_namespace,onto2_namespace)

    val threshold = parameters.getOrElse("threshold",0.8)

    val remove_correlated_threshold = parameters.getOrElse("correlation_threshold",0.5)
    println("Start element Level Matching")
    val individual_matcher_results:FeatureVector = matchAllIndividualMatchers(problem)
    println("Element Level Matching Done")
    println("Start remove correlated")
    val uncorrelated_matcher_results:FeatureVector = removeCorrelatedMatchers(individual_matcher_results,remove_correlated_threshold)
    println(" remove correlated done")
    val structural_matcher_results:Option[FeatureVector] =  matchAllStructuralMatchers(problem,uncorrelated_matcher_results)

    val outlier_analysis_vector:FeatureVector = if(structural_matcher_results.isDefined) VectorUtil.combineFeatureVectors(List(individual_matcher_results,structural_matcher_results.get),problem.name).get else individual_matcher_results
    println("Start Outlier analysis")

    val outlier_analysis_result: Map[MatchRelation, Double] =  RapidminerJobs.rapidminerOutlierDetectionNormalized(problem.name) (outlier_analysis_vector)

    println("Outlier Analysis Done")


    val namespace_filtered = MatchingPruner.nameSpaceFilter(outlier_analysis_result, allowed_namespaces)
    //now perform outlier analysis
    println("Namespace Filtering done")
    val selected =  MatchingSelector.greedyRankSelector(namespace_filtered,threshold)
    println("Greedy Rank Selection done")

    val alignment = new Alignment(null,null, selected)
    (alignment,outlier_analysis_vector)
  }



  /**
   * Calls an Individual matcher, for feature vector creation
   * @param matcher
   * @param problem
   * @return
   */
  def matchIndividualMatcher(matcher:Matcher, problem: MatchingProblem):  Map[MatchRelation, Double] = {
    matcher.align(problem,0.0).asMatchRelationMap()
  }

  /**
   *  Method that matches all individual matcher and consequently returns a Feature Vector with it's results
   * @param problem
   * @return
   */
  def matchAllIndividualMatchers(problem:MatchingProblem):FeatureVector = {
    val vector: ParMap[String, Map[MatchRelation, Double]] = MatcherRegistry.matcher_by_name.par.map({case (name,matcher) => {
      (name,matchIndividualMatcher(matcher, problem))}}) toMap

    val matcher_name_to_index: Map[String, Int] = vector.keys.toList.zipWithIndex.toMap
    val matcher_index_to_name:Map[Int,String] = matcher_name_to_index.map(tuple => (tuple._2,tuple._1)).toMap
    val vector_per_matchings = VectorUtil.createInvertedVector(vector.seq)

    FeatureVector(problem.name,vector.seq,vector_per_matchings,matcher_name_to_index,matcher_index_to_name)
  }

  /**
   * Performs structural matching for all available structural matcher with all remaining initial mappings
   * @param problem
   * @param featureVector
   * @return
   */
  def matchAllStructuralMatchers(problem:MatchingProblem, featureVector:FeatureVector): Option[FeatureVector] ={

    val results: List[FeatureVector] = MatcherRegistry.structural_matcher_by_name.map { case matcher =>
      matchStructuralMatcher(matcher._2, problem, featureVector)
    }.toList
    //combine and return results
    VectorUtil.combineFeatureVectors(results,problem.name)

  }

  /**
   * Matches one Structural matcher for all available initial mappings
   * @param matcher
   * @param problem
   * @param featureVector
   * @return
   */
  def matchStructuralMatcher(matcher:StructuralLevelMatcher, problem:MatchingProblem,featureVector:FeatureVector):FeatureVector = {
    //TODO add parallelism
    val results: Map[String, Map[MatchRelation, Double]] =  for(matcher_res <- featureVector.vector) yield {
      (matcher_res._1+matcher.getClass.getName.replace(".",""), matchStructuralMatcherWithAlignment(matcher,problem,matcher_res._2))
    }
    //return a feature vector
    VectorUtil.createVectorFromResult(results,problem.name)
  }

  /**
   * Matches one structural matcher for one initial mapping
   * @param matcher
   * @param problem
   * @param correspondences
   * @return
   */
  def matchStructuralMatcherWithAlignment(matcher:StructuralLevelMatcher, problem:MatchingProblem,correspondences:Map[MatchRelation,Double]): Map[MatchRelation, Double] ={
    //create alignment frm correspondences
    val initial_alignment = new Alignment(null,null,0.0, correspondences)

    matcher.align(problem,initial_alignment,0.0).asMatchRelationMap()
  }


  /**
   * Method to remove correlated feature for structural matchers
   *
   * @param feature_vector
   * @return
   */
  def removeCorrelatedMatchers(feature_vector:FeatureVector,threshold:Double) = {
    SparkJobs.removeCorrelatedFeatures(feature_vector,threshold)
  }



  def performMatcherAggregation(feature_vector: FeatureVector):Map[MatchRelation,String] = {
    null
  }




}

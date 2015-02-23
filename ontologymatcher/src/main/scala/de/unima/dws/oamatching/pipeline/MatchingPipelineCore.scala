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


  def createMatchingPipeline(outlierFct: (String,FeatureVector)=>(Int,Map[MatchRelation,Double])) (normFct: (Int,Iterable[(MatchRelation,Double)])=>Iterable[(MatchRelation,Double)]): (MatchingProblem, Double, Double) => (Alignment, FeatureVector) = {
    matchProblem(outlierFct)(normFct)
  }
  /**
   * To execute matching process
   * @param problem

   * @return
   */
  def matchProblem(outlierFct: (String,FeatureVector)=>(Int,Map[MatchRelation,Double])) (normFct: (Int,Iterable[(MatchRelation,Double)])=>Iterable[(MatchRelation,Double)])(problem: MatchingProblem,threshold:Double, remove_correlated_threshold:Double): (Alignment,FeatureVector) = {
    val start_time = System.currentTimeMillis()
    val runtime = Runtime.getRuntime
    val mb = 1024*1024
    val onto1_namespace = problem.ontology1.getOntologyID.getOntologyIRI.get().toString
    val onto2_namespace = problem.ontology2.getOntologyID.getOntologyIRI.get().toString
    println(onto1_namespace)
    println(onto2_namespace)
    val allowed_namespaces = List(onto1_namespace,onto2_namespace)

    println("Start element Level Matching")
    println("RAM Used " + ((runtime.totalMemory - runtime.freeMemory)/mb))
    val individual_matcher_results:FeatureVector = matchAllIndividualMatchers(problem)
    println("Element Level Matching Done")
    println("RAM Used " + ((runtime.totalMemory - runtime.freeMemory)/mb))
    println("Start remove correlated")
    val uncorrelated_matcher_results:FeatureVector = removeCorrelatedMatchers(individual_matcher_results,remove_correlated_threshold)
    println(" remove correlated done")
    println("RAM Used " + ((runtime.totalMemory - runtime.freeMemory)/mb))
    val structural_matcher_results:Option[FeatureVector] =  matchAllStructuralMatchers(problem,uncorrelated_matcher_results)

    val outlier_analysis_vector:FeatureVector = if(structural_matcher_results.isDefined) VectorUtil.combineFeatureVectors(List(individual_matcher_results,structural_matcher_results.get),problem.name).get else individual_matcher_results


    val filtered_outlier_analysis_vector:FeatureVector = MatchingPruner.featureVectorNameSpaceFilter(outlier_analysis_vector, allowed_namespaces)

    println("RAM Used " + ((runtime.totalMemory - runtime.freeMemory)/mb))
    println("Start Outlier analysis")
    val outlier_analysis_result: (Int, Map[MatchRelation, Double]) =  outlierFct(problem.name , filtered_outlier_analysis_vector)

    println("Outlier Analysis Done")
    println("RAM Used " + ((runtime.totalMemory - runtime.freeMemory)/mb))

    println("Total Execution Time: " +(System.currentTimeMillis() -start_time))

    //dimensionality normalization

    val alignment: Alignment = postProcessMatchings(normFct, threshold, outlier_analysis_result)


    (alignment,filtered_outlier_analysis_vector)
  }


  def postProcessMatchings(normFct: (Int, Iterable[(MatchRelation, Double)]) => Iterable[(MatchRelation, Double)], threshold: Double, outlier_analysis_result: (Int, Map[MatchRelation, Double])): Alignment = {
    val final_result: Iterable[(MatchRelation, Double)] = normFct.tupled(outlier_analysis_result)
    /*
    val final_result: Iterable[(MatchRelation, Double)] = if(do_dim_norm > 0.0){

      val dims = outlier_analysis_result._1

      println(dims)
      val maxDistance = Math.sqrt(dims.toDouble*4)
      println("dim norm " + maxDistance)
      outlier_analysis_result._2.view.map{case (match_relation,distance) =>{
        (match_relation, (distance/maxDistance))
      }

      }
    }else outlier_analysis_result._2*/


    //now perform outlier analysis
    println("Namespace Filtering done")
    println(threshold)
    val selected = MatchingSelector.greedyRankSelectorSimple(final_result.toMap, threshold)
    println("Greedy Rank Selection done")

    val alignment = new Alignment(null, null, selected)
    alignment
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

    val results: List[FeatureVector] = MatcherRegistry.structural_matcher_by_name.par.map { case matcher =>
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
    val starttime = System.currentTimeMillis()
    val initial_alignment = new Alignment(null,null,0.0, correspondences)

    val res =  matcher.align(problem,initial_alignment,0.0).asMatchRelationMap()
    //println(matcher.getClass.getSimpleName + " took "+ (System.currentTimeMillis()-starttime) +" ms")
    res
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
}

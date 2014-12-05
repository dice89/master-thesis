package de.unima.dws.omatching.pipeline

import java.net.URI
import java.util.Properties
import scala.collection.immutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.{ Map => MutableMap }
import org.semanticweb.owl.align.AlignmentProcess
import fr.inrialpes.exmo.align.impl.BasicParameters
import fr.inrialpes.exmo.align.impl.method.StringDistAlignment
import de.unima.dws.omatching.matcher.BaseMatcher
import de.unima.dws.oamatching.measures.StringMeasureHelper
import fr.inrialpes.exmo.ontosim.string.StringDistances
import de.unima.dws.omatching.matcher.URIFragmentStringMatcher
import org.semanticweb.owl.align.Alignment
import fr.inrialpes.exmo.align.impl.eval.PRecEvaluator
import de.unima.dws.omatching.matcher.MatchRelation

object MatcherRegistry {
  val matcher_by_name: MutableMap[String, BaseMatcher] = new HashMap[String, BaseMatcher]();

  /**
   * Method inits Map with matcher
   * @return
   */
  def init = {
    //Matcher Based on Alignment API String Matcher

    matcher_by_name += init_uri_fragment_string_matcher("hammingDistance");
    matcher_by_name += init_uri_fragment_string_matcher("jaroWinklerMeasure");
    matcher_by_name += init_uri_fragment_string_matcher("jaroMeasure");
    matcher_by_name += init_uri_fragment_string_matcher("levenshteinDistance");
    matcher_by_name += init_uri_fragment_string_matcher("needlemanWunsch2Distance");
    matcher_by_name += init_uri_fragment_string_matcher("ngramDistance");
    matcher_by_name += init_uri_fragment_string_matcher("smoaDistance");
    matcher_by_name += init_uri_fragment_string_matcher("subStringDistance");
    matcher_by_name += init_uri_fragment_string_matcher("equalDistance");
  }

  def init_uri_fragment_string_matcher(measure: String): (String, BaseMatcher) = {
    val name: String = measure + "uri_fragment_matcher";
    //TODO based on the measure name change 

     def measure_fct = get_matching_function(measure)
    (name, new URIFragmentStringMatcher(measure, measure_fct));
  }
  
  
  def get_matching_function(measure:String) = {
    measure match  {
      case "hammingDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.hammingDistance)
      case "jaroWinklerMeasure" => StringMeasureHelper.distance_lower_cased(StringDistances.jaroWinklerMeasure)
      case "jaroMeasure" => StringMeasureHelper.distance_lower_cased(StringDistances.jaroMeasure)
      case "levenshteinDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.levenshteinDistance)
      case "needlemanWunsch2Distance" => StringMeasureHelper.distance_lower_cased(StringDistances.needlemanWunsch2Distance)
      case "ngramDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.ngramDistance)
      case "smoaDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.smoaDistance)
      case "subStringDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.subStringDistance)
      case "equalDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.equalDistance)
      case "_" => StringMeasureHelper.distance_lower_cased(StringDistances.equalDistance) //default is equal distance
    }
  }

  def matchRound(onto1: URI, onto2: URI, reference: Alignment): (Map[MatchRelation, Map[String, Option[Double]]],Map[String,EvaluationResult]) = {
    println("Start Matching with " + matcher_by_name.size + " Matcher")
    
     //call all matcher and transform them
     val res_map= matcher_by_name.map({
      case (name, matcher) => {
        matcher.prepare(onto1, onto2)
        (name,matcher.align(0.5))
      } 
    }) toMap

    
    val eval_map = matcher_by_name.map({
      case (name, matcher) => {
         val p_rec_eval = new PRecEvaluator(reference, matcher);
     
         p_rec_eval.eval(null)
         val res = EvaluationResult(p_rec_eval.getPrecision(),p_rec_eval.getRecall(),p_rec_eval.getFmeasure())
         println(res)
        (name,res)
      }
    }  
    ) toMap
    

    //get all unique matches 
    val unique_elements = res_map.map(tuple => tuple._2.keySet).flatten;

    //get per unique matches a Map of matching results
    val results_per_matching = unique_elements.map(elm => elm -> res_map.filter(_._2.contains(elm)).map(tuple => tuple._1 -> tuple._2.get(elm))) toMap

    println("Done with "+matcher_by_name .size+" Matcher and created matchings for " + results_per_matching.size)

    (results_per_matching,eval_map)
  }

}
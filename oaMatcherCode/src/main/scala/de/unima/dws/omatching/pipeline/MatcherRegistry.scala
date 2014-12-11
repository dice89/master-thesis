package de.unima.dws.omatching.pipeline

import java.net.URI
import scala.collection.immutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.{ Map => MutableMap }
import org.semanticweb.owl.align.Alignment
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology
import com.wcohen.ss.JaroWinkler
import com.wcohen.ss.SoftTFIDF
import com.wcohen.ss.TFIDF
import com.wcohen.ss.tokens.SimpleTokenizer
import de.unima.dws.oamatching.matcher.NameAndPropertyAlignment
import de.unima.dws.oamatching.matcher.StrucSubsDistAlignment
import de.unima.dws.oamatching.measures.SimpleMeasures
import de.unima.dws.oamatching.measures.StandardMeasure
import de.unima.dws.oamatching.measures.StringMeasureHelper
import de.unima.dws.oamatching.measures.TrainedMeasure
import de.unima.dws.oamatching.measures.base.SecondStringTokenMatcher
import de.unima.dws.oamatching.measures.base.StringFunctionMatcher
import de.unima.dws.oamatching.util.wordnet.LinWordMatching
import de.unima.dws.omatching.matcher.BaseMatcher
import de.unima.dws.omatching.matcher.MatchRelation
import de.unima.dws.omatching.matcher.PostPrunedMatcher
import fr.inrialpes.exmo.align.impl.eval.PRecEvaluator
import fr.inrialpes.exmo.ontosim.string.StringDistances
import com.wcohen.ss.JaroWinklerTFIDF
import com.wcohen.ss.Level2Jaro
import com.wcohen.ss.Level2JaroWinkler
import com.wcohen.ss.Level2Levenstein
import com.wcohen.ss.Level2MongeElkan
import de.unima.dws.oamatching.util.wordnet.JiangConrath

object MatcherRegistry {
  val matcher_by_name: MutableMap[String, BaseMatcher] = new HashMap[String, BaseMatcher]();

  /**
   * Method inits Map with matcher
   * @return
   */
  def init = {
    //init simple string metrics

    matcher_by_name += init_uri_fragment_string_distance_matcher("hammingDistance");
    matcher_by_name += init_uri_fragment_string_distance_matcher("jaroWinklerMeasure");
    matcher_by_name += init_uri_fragment_string_distance_matcher("jaroMeasure");
    matcher_by_name += init_uri_fragment_string_distance_matcher("levenshteinDistance");
    matcher_by_name += init_uri_fragment_string_distance_matcher("needlemanWunsch2Distance");
    matcher_by_name += init_uri_fragment_string_distance_matcher("ngramDistance");
    matcher_by_name += init_uri_fragment_string_distance_matcher("smoaDistance");
    matcher_by_name += init_uri_fragment_string_distance_matcher("subStringDistance");
    matcher_by_name += init_uri_fragment_string_distance_matcher("equalDistance");
    matcher_by_name += init_uri_fragment_string_similarity_matcher("jiangConrath",true);
    matcher_by_name += init_uri_fragment_string_similarity_matcher("jaccardStemmed", true);
    matcher_by_name += init_uri_fragment_string_similarity_matcher("lin", false);
    matcher_by_name += init_uri_fragment_string_similarity_matcher("mongeElkan", false);
    //init token based matchers

    //compose preprocessing function
    //maybe switch to Helper class
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _
    val simple_preprocessing = Function.untupled(tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem _ compose (StringMeasureHelper.getLabel _).tupled)
    // extractor takes now comments and referencing object and data properties into account
    val advanced_preprocessing = Function.untupled(tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem _ compose (StringMeasureHelper.getLabelAndProperties _).tupled)

    //build measures
    val tfidf_matcher = new TrainedMeasure(true, new SecondStringTokenMatcher(simple_preprocessing, new TFIDF(new SimpleTokenizer(true, false))))  
    matcher_by_name += ("simple_tfidf" -> new PostPrunedMatcher("simple_tfidf", tfidf_matcher))
 
    val soft_tfidf_matcher = new TrainedMeasure(true, new SecondStringTokenMatcher(simple_preprocessing, new SoftTFIDF(new SimpleTokenizer(true, false), new JaroWinkler(), 0.9)))
    matcher_by_name += ("soft_tfidf_jaro" -> new PostPrunedMatcher("simple_soft_tfidf_jaro", soft_tfidf_matcher))

    val jaro_tfidf = new TrainedMeasure(true, new SecondStringTokenMatcher(simple_preprocessing,  new JaroWinklerTFIDF()))  
    matcher_by_name += ("jaro_tfidf" -> new PostPrunedMatcher("jaro_tfidf", jaro_tfidf))

    val level2_jaro = new TrainedMeasure(true, new SecondStringTokenMatcher(simple_preprocessing, new Level2Jaro()))
    matcher_by_name += ("level2_jaro" -> new PostPrunedMatcher("level2_jaro", level2_jaro))

    val level2_jaro_winkler = new TrainedMeasure(true, new SecondStringTokenMatcher(simple_preprocessing, new Level2JaroWinkler()))
    matcher_by_name += ("level2_jaro_winkler" -> new PostPrunedMatcher("level2_jaro_winkler", level2_jaro_winkler))
    

     val level2_monge_elkan = new TrainedMeasure(true, new SecondStringTokenMatcher(simple_preprocessing, new Level2MongeElkan()))
    matcher_by_name += ("level2_monge_elkan" -> new PostPrunedMatcher("level2_monge_elkan", level2_monge_elkan))
    

    val level2_leven = new TrainedMeasure(true, new SecondStringTokenMatcher(simple_preprocessing,new Level2Levenstein()))
    matcher_by_name += ("level2_leven" -> new PostPrunedMatcher("level2_leven", level2_leven))

    
    //with extended preprocessing
     val advanced_tfidf_matcher = new TrainedMeasure(true, new SecondStringTokenMatcher(advanced_preprocessing, new TFIDF(new SimpleTokenizer(true, false))))  
    matcher_by_name += ("advanced_tfidf" -> new PostPrunedMatcher("advanced_tfidf", advanced_tfidf_matcher))
  

    //structural Matcher from alignment API

    matcher_by_name += ("class_struct_alignment_api" -> new StrucSubsDistAlignment())
    matcher_by_name += ("name_and_property_alignment_api" -> new NameAndPropertyAlignment())
  }

  def init_uri_fragment_string_distance_matcher(measure: String): (String, BaseMatcher) = {
    val name: String = measure + "_matcher";
    val measure_fct: (String, String) => Double = get_string_matching_function(measure)

    val stringfunction = new StandardMeasure(false, new StringFunctionMatcher(StringMeasureHelper.getLabel _, measure_fct))
    (name, new PostPrunedMatcher(measure, stringfunction));
  }

  def init_uri_fragment_string_similarity_matcher(measure: String, tokenized: Boolean): (String, BaseMatcher) = {

    val name: String = measure + "_matcher";
    def measure_fct = get_string_matching_function(measure)

    var preprocess: (OWLEntity, OWLOntology) => String = null
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _

    if (tokenized) {
      preprocess = Function.untupled(tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem _ compose (StringMeasureHelper.getLabel _).tupled)

    } else {
      preprocess = StringMeasureHelper.getLabel
    }

    val stringfunction = new StandardMeasure(true, new StringFunctionMatcher(StringMeasureHelper.getLabel _, measure_fct))

    (name, new PostPrunedMatcher(measure, stringfunction));
  }

  def get_string_matching_function(measure: String) = {

    measure match {
      case "hammingDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.hammingDistance)
      case "jaroWinklerMeasure" => StringMeasureHelper.distance_lower_cased(StringDistances.jaroWinklerMeasure)
      case "jaroMeasure" => StringMeasureHelper.distance_lower_cased(StringDistances.jaroMeasure)
      case "levenshteinDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.levenshteinDistance)
      case "needlemanWunsch2Distance" => StringMeasureHelper.distance_lower_cased(StringDistances.needlemanWunsch2Distance)
      case "ngramDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.ngramDistance)
      case "smoaDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.smoaDistance)
      case "subStringDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.subStringDistance)
      case "equalDistance" => StringMeasureHelper.distance_lower_cased(StringDistances.equalDistance)
      case "lin" => StringMeasureHelper.distance_lower_cased(new LinWordMatching().getSimScoreTokenized)
      case "jiangConrath" => StringMeasureHelper.distance_lower_cased(new JiangConrath().getSimScoreTokenized)
      case "jaccardStemmed" => StringMeasureHelper.distance_lower_cased(SimpleMeasures.computeJaccard)
      case "mongeElkan" => StringMeasureHelper.distance_lower_cased(SimpleMeasures.computeMongeElkan)
      case "_" => StringMeasureHelper.distance_lower_cased(StringDistances.equalDistance) //default is equal distance

    }
  }
  

  def matchRound(onto1: URI, onto2: URI, reference: Alignment): (Map[MatchRelation, Map[String, Option[Double]]], Map[String, EvaluationResult]) = {
    println("Start Matching with " + matcher_by_name.size + " Matcher" + onto1 + onto2)

    //call all matcher and transform them
    val res_map = matcher_by_name.map({
      case (name, matcher) => {
        matcher.prepare(onto1, onto2)
        (name, matcher.align(0.5))
      }
    }) toMap

    val eval_map = matcher_by_name.map({
      case (name, matcher) => {
        val p_rec_eval = new PRecEvaluator(reference, matcher);

        p_rec_eval.eval(null)
        val res = EvaluationResult(p_rec_eval.getPrecision(), p_rec_eval.getRecall(), p_rec_eval.getFmeasure())
        println((name, res))
        (name, res)
      }
    }) toMap

    //get all unique matches 
    val unique_elements = res_map.map(tuple => tuple._2.keySet).flatten;

    println(eval_map.keySet)
    //get per unique matches a Map of matching results
    val results_per_matching = unique_elements.map(elm => elm -> res_map.filter(_._2.contains(elm)).map(tuple => tuple._1 -> tuple._2.get(elm))) toMap

    println("Done with " + matcher_by_name.size + " Matcher and created matchings for " + results_per_matching.size)

    (results_per_matching, eval_map)
  }

}
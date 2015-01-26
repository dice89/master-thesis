package de.unima.dws.oamatching.matcher

import com.wcohen.ss._
import com.wcohen.ss.tokens.SimpleTokenizer
import de.unima.dws.oamatching.core.matcher.{SimilarityFloodingMatcher, StructuralLevelMatcher, Matcher, ElementLevelMatcher}
import de.unima.dws.oamatching.matcher.elementlevel.{TokenizedStringMatcher, TrainedSecondStringMatcher, PreProcessedStringMatcher, SimpleStringFunctionMatcher}
import de.unima.dws.oamatching.measures.{StringMeasureHelper, StringMeasures}
import edu.cmu.lti.ws4j.impl.JiangConrath
import fr.inrialpes.exmo.ontosim.string.StringDistances
import org.semanticweb.owlapi.model.{OWLEntity, OWLOntology}

import scala.collection.mutable.{HashMap, Map => MutableMap}

object MatcherRegistry {
  val matcher_by_name: MutableMap[String, Matcher] = new HashMap[String, Matcher]()

  val structural_matcher_by_name : MutableMap[String, StructuralLevelMatcher] = new HashMap[String, StructuralLevelMatcher]()


  def initStructuralMatcher() = {
    structural_matcher_by_name += ( ("simFloodMatcher",new SimilarityFloodingMatcher()))
  }


  /**
   * Method inits Map with matcher
   * @return
   */
  def init = {
    //init Structural Matcher
    initStructuralMatcher()
    //init simple string metrics
    matcher_by_name += init_uri_fragment_string_distance_matcher("hammingDistance")
    matcher_by_name += init_uri_fragment_string_distance_matcher("jaroWinklerMeasure")
    matcher_by_name += init_uri_fragment_string_distance_matcher("jaroMeasure")
    matcher_by_name += init_uri_fragment_string_distance_matcher("levenshteinDistance")
    matcher_by_name += init_uri_fragment_string_distance_matcher("needlemanWunsch2Distance")
    matcher_by_name += init_uri_fragment_string_distance_matcher("ngramDistance")
    matcher_by_name += init_uri_fragment_string_distance_matcher("smoaDistance")
    matcher_by_name += init_uri_fragment_string_distance_matcher("subStringDistance")
    matcher_by_name += init_uri_fragment_string_distance_matcher("equalDistance")
    matcher_by_name += init_uri_fragment_string_similarity_matcher("prefix", tokenized = false)
    matcher_by_name += init_uri_fragment_string_similarity_matcher("suffix", tokenized = false)
    //matcher_by_name += init_uri_fragment_string_similarity_matcher("jiangConrath", true);
    matcher_by_name += init_uri_fragment_string_similarity_matcher("jaccardStemmed", tokenized = true)
    //matcher_by_name += init_uri_fragment_string_similarity_matcher("lin", false);
    matcher_by_name += init_uri_fragment_string_similarity_matcher("mongeElkan", tokenized = false)
    //init token based matchers

    //compose preprocessing function
    //maybe switch to Helper class
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _
    val simple_preprocessing = StringMeasureHelper.to_lower_case_single _ compose tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem  compose StringMeasureHelper.minimalPreprocess
    // extractor takes now comments and referencing object and data properties into account


    //add language based measures

    //tfidf
    matcher_by_name += ("simple_tfidf" -> new TrainedSecondStringMatcher(true,simple_preprocessing, new TFIDF(new SimpleTokenizer(true, false))))

    matcher_by_name += ("soft_tfidf_jaro" -> new TrainedSecondStringMatcher(true,simple_preprocessing,new SoftTFIDF(new SimpleTokenizer(true, false), new JaroWinkler(), 0.8)))

    matcher_by_name += ("jaro_winkler_tfidf" -> new TrainedSecondStringMatcher(true,simple_preprocessing,new JaroWinklerTFIDF()))

    matcher_by_name += ("level2_jaro" -> new TrainedSecondStringMatcher(true,simple_preprocessing,new Level2Jaro()))

    matcher_by_name += ("level2_jara_winkler" -> new TrainedSecondStringMatcher(true,simple_preprocessing,new Level2JaroWinkler()))

    matcher_by_name += ("level2_monge_elkan" -> new TrainedSecondStringMatcher(true,simple_preprocessing,new Level2MongeElkan()))

    matcher_by_name += ("lin" ->  new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess,tokenizer, StringMeasures.computeLin ))

    matcher_by_name += ("path" ->  new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess,tokenizer, StringMeasures.computePath ))

    matcher_by_name += ("jiangConrath" ->  new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess,tokenizer, StringMeasures.computeJiangConrath ))

    matcher_by_name += ("wuPalmer" ->  new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess,tokenizer, StringMeasures.computeWuPalmer ))

    //TODO add structural matcher
  }

  /**
   * @param measure
   * @return
   */
  def init_uri_fragment_string_distance_matcher(measure: String): (String, SimpleStringFunctionMatcher) = {
    val name: String = measure
    val measure_fct: (String, String) => Double = get_string_matching_function(measure)

    (name, new SimpleStringFunctionMatcher(false, measure_fct))
  }

  /**
   * @param measure
   * @param tokenized
   * @return
   */
  def init_uri_fragment_string_similarity_matcher(measure: String, tokenized: Boolean): (String, Matcher) = {

    val name: String = measure + "_matcher"
    def measure_fct = get_string_matching_function(measure)

    var preprocess: (String) => String = null
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _

    if (tokenized) {
      preprocess = tokens_to_string compose  tokenizer compose StringMeasureHelper.porter_stem  compose StringMeasureHelper.minimalPreprocess
    } else {
      preprocess = StringMeasureHelper.minimalPreprocess
    }

    (name,new PreProcessedStringMatcher(true, preprocess, measure_fct))

  }

  /**
   * @param measure
   * @return
   */
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

      //case "lin" => StringMeasureHelper.distance_lower_cased(new LinWordMatching().getSimScoreTokenized)
      //case "jiangConrath" => StringMeasureHelper.distance_lower_cased(new JiangConrath().getSimScoreTokenized)


      case "jaccardStemmed" => StringMeasureHelper.distance_lower_cased(StringMeasures.computeJaccard)
      case "mongeElkan" => StringMeasureHelper.distance_lower_cased(StringMeasures.computeMongeElkan)
      case "prefix" => StringMeasureHelper.distance_lower_cased(StringMeasures.computePrefixBiDirectional)
      case "suffix" => StringMeasureHelper.distance_lower_cased(StringMeasures.computeSuffixBiDirectional)
      case "_" => StringMeasureHelper.distance_lower_cased(StringDistances.equalDistance) //default is equal distance

    }
  }


  /**
   * @param name
   * @return
   */
  def getMatcherByName(name: String): Option[Matcher] = {
    matcher_by_name.get(name)
  }

}
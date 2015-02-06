package de.unima.dws.oamatching.matcher

import com.github.tototoshi.csv.CSVReader
import com.wcohen.ss._
import com.wcohen.ss.tokens.SimpleTokenizer
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.matcher.{Matcher, StructuralLevelMatcher}
import de.unima.dws.oamatching.matcher.elementlevel.{PreProcessedStringMatcher, SimpleStringFunctionMatcher, TokenizedStringMatcher, TrainedSecondStringMatcher}
import de.unima.dws.oamatching.matcher.structurallevel.{GraphBasedUsedClassMatcher, GraphBasedUsedPropertyMatcher, SimilarityFloodingMatcher}
import de.unima.dws.oamatching.measures.{SemanticMeasures, StringMeasureHelper, StringMeasures}
import fr.inrialpes.exmo.ontosim.string.StringDistances

import scala.collection.mutable.{Map => MutableMap}

object MatcherRegistry {

  //testing
  val KEY_SIM = "sim"
  val KEY_DIS = "dis"

  val KEY_ELEM = "E"
  //element level matcher that do only need a string fct
  val KEY_STRUCT = "S"
  // key structural matcher
  val KEY_TRAINED = "T" //key for second string matcher

  val KEY_STEMMED = "stemmed"

  //element level matcher
  var matcher_by_name: Map[String, Matcher] = Map[String, Matcher]()


  //structural level matcher
  var structural_matcher_by_name: Map[String, StructuralLevelMatcher] = Map[String, StructuralLevelMatcher]()

  def initStructuralMatcher() = {
    structural_matcher_by_name += (("simFloodMatcher", new SimilarityFloodingMatcher()))
    structural_matcher_by_name += (("graphBasedUsedPropMatcher", new GraphBasedUsedPropertyMatcher()))
    structural_matcher_by_name += (("graphBasedUsedClassMatcher", new GraphBasedUsedClassMatcher()))
  }

  /**
   * Init the available matcher from a config file
   * @param path
   */
  def initFromConfigFile(path: String) = {

    val reader = CSVReader.open(path)

    val mapped_values = reader.allWithHeaders.foreach(row => {
      val matcher_name = row.get("name").get
      val matcher_type = row.get("type").get
      val sim_dis = row.get("sim_type").getOrElse("sim")
      val tokenized = row.get("tokenized").getOrElse("false").toBoolean
      val string_norm = row.get("normalized").getOrElse("none")

      val matcher = initMatcher(matcher_name, matcher_type, sim_dis, tokenized, string_norm)

      if (matcher.isDefined) {
        if (matcher_type.equals(KEY_STRUCT)) {
          structural_matcher_by_name += ((matcher_name, matcher.get.asInstanceOf[StructuralLevelMatcher]))
        } else {
          matcher_by_name += ((matcher_name, matcher.get))
        }
      }
    })

    println("Initialized: "+ (matcher_by_name.size + structural_matcher_by_name.size))
  }

  def initSmallScale() = {
    initFromConfigFile(Config.PATH_TO_SMALL_SCALE_CONFIG)
  }

  def initLargeScale() = {
    initFromConfigFile(Config.PATH_TO_LARGE_SCALE_CONFIG)
  }

  def initMatcher(matcher_name: String, matcher_type: String, sim_dis: String, tokenized: Boolean, string_norm: String): Option[Matcher] = {

    val matcher = matcher_type match {
      case "E" => Option(initElementLevelMatcher(matcher_name, sim_dis, tokenized, string_norm))
      case "S" => Option(initStructuralMatcher(matcher_name))
      case "T" => Option(initTrainedMatcher(matcher_name))
      case _ => Option.empty
    }
    matcher
  }


  /**
   * Inits an Element level Matcher based on the given configuration
   * @param matcher_name
   * @param sim_dis
   * @param tokenized
   * @param string_norm
   * @return Matcher initialized
   */
  def initElementLevelMatcher(matcher_name: String, sim_dis: String, tokenized: Boolean, string_norm: String): Matcher = {
    val is_similarity = sim_dis match {
      case KEY_DIS => false
      case KEY_SIM => true
      case _ => true
    }
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val stemmed_tokenizer = tokenizer compose StringMeasureHelper.porter_stem compose StringMeasureHelper.minimalPreprocess
    //get function
    val measure_fct: (String, String) => Double = get_string_matching_function(matcher_name)

    tokenized match {
      case false => new SimpleStringFunctionMatcher(is_similarity, measure_fct)
      case true => {
        string_norm match {
          case KEY_STEMMED => new TokenizedStringMatcher(is_similarity, StringMeasureHelper.minimalPreprocess, stemmed_tokenizer, measure_fct)
          case _ => new TokenizedStringMatcher(is_similarity, StringMeasureHelper.minimalPreprocess, tokenizer, measure_fct)
        }
      }
    }

  }

  /**
   * Return an newly initialized structural Level Matcher
   * @param matcher_name
   * @return
   */
  def initStructuralMatcher(matcher_name: String): StructuralLevelMatcher = {
    matcher_name match {
      case "simFloodMatcher" => new SimilarityFloodingMatcher()
      case "graphBasedUsedPropMatcher" => new GraphBasedUsedPropertyMatcher()
      case "graphBasedUsedClassMatcher" => new GraphBasedUsedClassMatcher()
    }
  }

  /**
   * Inits an Second String based matcher ignores all other parameters
   * @param matcher_name
   * @return
   */
  def initTrainedMatcher(matcher_name: String): Matcher = {
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _
    val simple_preprocessing = StringMeasureHelper.to_lower_case_single _ compose tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem compose StringMeasureHelper.minimalPreprocess

    val kernel_second_string_fct = getSecondTrainingFunction(matcher_name)
    new TrainedSecondStringMatcher(true, simple_preprocessing, kernel_second_string_fct)
  }



  /**
   * @param measure
   * @return
   */
  def get_string_matching_function(measure: String): (String, String) => Double = {

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
      case "word2Vec" => StringMeasureHelper.distance_lower_cased(SemanticMeasures.word2VecSimilarityMeasure)
      case "word2VecStemmed" => StringMeasureHelper.distance_lower_cased(SemanticMeasures.word2VecSimilarityMeasureStemmed)
      case "umbcSim" => StringMeasureHelper.distance_lower_cased(SemanticMeasures.umbcSim)
      case "umbcphrasesim" => StringMeasureHelper.distance_lower_cased(SemanticMeasures.callPhraseSimServiceUMBCRegular)
      case "umbcstssim" => StringMeasureHelper.distance_lower_cased(SemanticMeasures.callSTSServiceUMBC)
      case "jaccardStemmed" => StringMeasureHelper.distance_lower_cased(StringMeasures.computeJaccard)
      case "mongeElkan" => StringMeasureHelper.distance_lower_cased(StringMeasures.computeMongeElkan)
      case "prefix" => StringMeasureHelper.distance_lower_cased(StringMeasures.computePrefixBiDirectional)
      case "suffix" => StringMeasureHelper.distance_lower_cased(StringMeasures.computeSuffixBiDirectional)
      case "lin" => StringMeasures.computeLin _
      case "path" => StringMeasures.computePath _
      case "jiangConrath" => StringMeasures.computeJiangConrath _
      case "wuPalmer" => StringMeasures.computeWuPalmer _
      case _ => StringMeasureHelper.distance_lower_cased(StringDistances.equalDistance) //default is equal distance

    }
  }

  def getSecondTrainingFunction(second_string_measure: String) = {
    second_string_measure match {
      case "simple_tfidf" => new TFIDF(new SimpleTokenizer(true, false))
      case "soft_tfidf_jaro" => new SoftTFIDF(new SimpleTokenizer(true, false), new JaroWinkler(), 0.8)
      case "jaro_winkler_tfidf" => new JaroWinklerTFIDF()
      case "level2_jaro" => new Level2Jaro()
      case "level2_jara_winkler" => new Level2JaroWinkler()
      case "level2_monge_elkan" => new Level2MongeElkan()
      case _ => new TFIDF(new SimpleTokenizer(true, false))

    }
  }


  /**
   * @param name
   * @return
   */
  def getMatcherByName(name: String): Option[Matcher] = {
    matcher_by_name.get(name)
  }

  //############################################## obsolete functions
  /**
   *
   * //obsolete
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
    matcher_by_name += init_uri_fragment_string_similarity_matcher("jaccardStemmed", tokenized = true)
    matcher_by_name += init_uri_fragment_string_similarity_matcher("mongeElkan", tokenized = false)

    //word2vec measure
    matcher_by_name += init_uri_fragment_tokenized_matcher("word2Vec", false)
    matcher_by_name += init_uri_fragment_tokenized_matcher("word2VecStemmed", true)
    matcher_by_name += init_uri_fragment_tokenized_matcher("umbcSim", false)
    //Umbc Matcher


    //init token based matchers
    //compose preprocessing function
    //maybe switch to Helper class
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _
    val simple_preprocessing = StringMeasureHelper.to_lower_case_single _ compose tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem compose StringMeasureHelper.minimalPreprocess
    // extractor takes now comments and referencing object and data properties into account


    //add language based measures
    //tfidf
    matcher_by_name += ("simple_tfidf" -> new TrainedSecondStringMatcher(true, simple_preprocessing, new TFIDF(new SimpleTokenizer(true, false))))

    matcher_by_name += ("soft_tfidf_jaro" -> new TrainedSecondStringMatcher(true, simple_preprocessing, new SoftTFIDF(new SimpleTokenizer(true, false), new JaroWinkler(), 0.8)))

    matcher_by_name += ("jaro_winkler_tfidf" -> new TrainedSecondStringMatcher(true, simple_preprocessing, new JaroWinklerTFIDF()))

    matcher_by_name += ("level2_jaro" -> new TrainedSecondStringMatcher(true, simple_preprocessing, new Level2Jaro()))

    matcher_by_name += ("level2_jara_winkler" -> new TrainedSecondStringMatcher(true, simple_preprocessing, new Level2JaroWinkler()))

    matcher_by_name += ("level2_monge_elkan" -> new TrainedSecondStringMatcher(true, simple_preprocessing, new Level2MongeElkan()))

    matcher_by_name += ("lin" -> new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess, tokenizer, StringMeasures.computeLin))

    matcher_by_name += ("path" -> new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess, tokenizer, StringMeasures.computePath))

    matcher_by_name += ("jiangConrath" -> new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess, tokenizer, StringMeasures.computeJiangConrath))

    matcher_by_name += ("wuPalmer" -> new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess, tokenizer, StringMeasures.computeWuPalmer))

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

    val name: String = measure
    def measure_fct = get_string_matching_function(measure)

    var preprocess: (String) => String = null
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _

    if (tokenized) {
      preprocess = tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem compose StringMeasureHelper.minimalPreprocess
    } else {
      preprocess = StringMeasureHelper.minimalPreprocess
    }

    (name, new PreProcessedStringMatcher(true, preprocess, measure_fct))

  }


  def init_uri_fragment_tokenized_matcher(measure: String, stemmed: Boolean): (String, Matcher) = {
    var preprocess: (String) => String = null
    def measure_fct = get_string_matching_function(measure)

    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _
    if (stemmed) {
      val stemmed_tokenizer = tokenizer compose StringMeasureHelper.porter_stem compose StringMeasureHelper.minimalPreprocess
      (measure, new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess, stemmed_tokenizer, measure_fct))

    } else {
      (measure, new TokenizedStringMatcher(true, StringMeasureHelper.minimalPreprocess, tokenizer, measure_fct))
    }


  }

}
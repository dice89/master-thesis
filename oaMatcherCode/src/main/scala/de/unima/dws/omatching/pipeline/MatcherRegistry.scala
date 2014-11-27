 package de.unima.dws.omatching.pipeline

import java.io.FileWriter
 import java.net.URI
 import java.util.Properties
 import scala.collection.immutable.{ Map => ImmutableMap }
 import scala.collection.mutable.HashMap
 import scala.collection.mutable.Map
 import org.apache.commons.csv.CSVFormat
 import org.apache.commons.csv.CSVPrinter
 import org.semanticweb.owl.align.AlignmentProcess
 import fr.inrialpes.exmo.align.impl.BasicParameters
 import fr.inrialpes.exmo.align.impl.method.StringDistAlignment
 import com.github.tototoshi.csv.CSVWriter
 import java.io.File

object MatcherRegistry {
  val matcher_by_name: Map[String, Matcher] = new HashMap[String, Matcher]();

  /**
   * Method inits Map with matcher
   * @return
   */
  def init = {
    //Matcher Based on Alignment API String Matcher

    matcher_by_name += init_simple_string_matcher("hammingDistance");
    matcher_by_name += init_simple_string_matcher("jaroWinklerMeasure");
    matcher_by_name += init_simple_string_matcher("jaroMeasure");
    matcher_by_name += init_simple_string_matcher("levenshteinDistance");
    matcher_by_name += init_simple_string_matcher("needlemanWunsch2Distance");
    matcher_by_name += init_simple_string_matcher("ngramDistance");
    matcher_by_name += init_simple_string_matcher("smoaDistance");
    matcher_by_name += init_simple_string_matcher("subStringDistance");
  }

  def init_simple_string_matcher(measure: String): (String, Matcher) = {
    val name: String = measure + "_matcher";

    var properties: Properties = new BasicParameters()
    properties.setProperty("stringFunction", measure)
    val alignment: AlignmentProcess = new StringDistAlignment()

    (name, new Matcher(name, alignment, properties));
  }

  def matchAll(onto1: URI, onto2: URI): ImmutableMap[String,Map[String,Option[Double]]] = {
    println("Start Matching with " + matcher_by_name.size + " Matcher")

    //call all matcher and transform them
    val res_map: Map[String, ImmutableMap[String, Double]] = matcher_by_name.map(tuple => tuple._1 -> tuple._2.o_match(onto1, onto2))

    //get all unique matches 
    val unique_elements = res_map.map(tuple => tuple._2.keySet).flatten;

    //get per unique matches a Map of matching results
    val results_per_matching = unique_elements.map(elm => elm -> res_map.filter(_._2.contains(elm)).map(tuple => tuple._1 -> tuple._2.get(elm))) toMap

    println("Done with 8 Matcher and created matchings for " + results_per_matching.size)
    
    results_per_matching
  }

}
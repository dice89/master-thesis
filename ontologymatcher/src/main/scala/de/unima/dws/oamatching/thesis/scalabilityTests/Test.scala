package de.unima.dws.oamatching.thesis.scalabilityTests

import com.typesafe.scalalogging.Logging
import com.typesafe.scalalogging.slf4j.{LazyLogging, Logger}
import de.unima.dws.alex.simservice.SimService
import de.unima.dws.oamatching.core.{AlignmentParser, AggregatedEvaluationResult}
import de.unima.dws.oamatching.measures.{SemanticMeasures, StringMeasureHelper, StringMeasures}
import de.unima.dws.oamatching.pipeline.MatchingPruner
import de.unima.dws.oamatching.thesis.{ResultServerHandling, SeparatedResult}
import org.slf4j.LoggerFactory
import play.api.libs.json.Json

import scala.collection.immutable.HashMap
import scalaj.http.{Http, HttpResponse}

/**
 * Created by mueller on 02/03/15.
 */


case class Tester(left: String, right: String, relation: String, measure: Double, owl_type: String)

object Test extends App with ResultServerHandling with LazyLogging {

  def test = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _


 /* println(StringMeasures.computeSuffixBiDirectional("Document","Conference_document"))
  println(StringMeasures.computeSuffixBiDirectional("Co-Author","Contribution_co-author"))
  println(StringMeasures.computeSuffixBiDirectional("Conference_volume","Conference"))
  println(StringMeasures.computePrefixBiDirectional("Conference_volume","Conference"))

  println(StringMeasures.computeLin("programm","programm"))

  println(StringMeasures.computeLin("subject","topic"))
  println(StringMeasures.computePath("subject","topic"))*/


   def scoreTokenized(tokens_a:List[String], tokens_b:List[String]):Double = {
    var summed_score:Double  =0.0
    var counter:Int = 0

    for (term_a <- tokens_a; term_b <- tokens_b) {
      counter= counter +1




      val a = StringMeasureHelper.porter_stem(term_a.toLowerCase())
      val b =  StringMeasureHelper.porter_stem(term_b.toLowerCase())

      println(a +"--"+b)
      val score = if(a.equals(b)) 1.0 else 0.0
      summed_score = summed_score + score
    }

    val res:Double =summed_score / Math.min(tokens_a.length,tokens_b.length)

    res
  }



  val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
  val tokens_to_string = StringMeasureHelper.token_list_to_String _

  val tokens_a = tokenizer.apply("has_title")
  val tokens_b = tokenizer.apply("hasTitle")

  val simple_preprocessing: (String) => String = StringMeasureHelper.to_lower_case_single _ compose tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem compose StringMeasureHelper.minimalPreprocess



  println(simple_preprocessing("has_title"))
  println(simple_preprocessing("hasTitle"))
  //println(tokens_to_string(tokens_b))

  //println(scoreTokenized(tokens_a,tokens_b))



}

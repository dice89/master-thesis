package de.unima.dws.oamatching.thesis.scalabilityTests

import com.typesafe.scalalogging.Logging
import com.typesafe.scalalogging.slf4j.{LazyLogging, Logger}
import de.unima.dws.oamatching.core.{AlignmentParser, AggregatedEvaluationResult}
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


  logger.info("test")

  val ref_file= "ontos/2014/conference/reference-alignment/cmt-edas.rdf"
  val onto1= "ontos/2014/conference/cmt.owl"
  val onto2= "ontos/2014/conference/edas.owl"

  val ref = AlignmentParser.parseRDFWithOntos(ref_file,onto1,onto2)
  println("parse_created")


  val created = AlignmentParser.parseRDFWithOntos("cmt-edas-best.rdf",onto1,onto2)

  println(created.evaluate(ref))

  val debugged = MatchingPruner.debugAlignment(created)

  println(debugged.evaluate(ref))

}

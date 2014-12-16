package de.uniman.dws.oamatching.logging

import de.unima.dws.omatching.pipeline.EvaluationResult
import com.github.tototoshi.csv.CSVWriter
import org.apache.log4j.PropertyConfigurator
import org.apache.log4j.Logger
import org.apache.log4j.Level

object ResultLogger {
  PropertyConfigurator.configure("log4j.properties");
  
  //text experiment logger
  val exp_logger = Logger.getLogger("experiment.log");

  
  //logging each value
  val matcher_writer = CSVWriter.open("matching_matcher_result.csv")
  matcher_writer.writeRow(List[String]("dataset", "matcher", "precision", "recall", "f1-measure"))

  val result_writer = CSVWriter.open("matching_matcher_result.csv")
  result_writer.writeRow(List[String]("dataset", "matcher", "precision", "recall", "f1-measure"))

  def log_matcher_result(dataset: String, matcher: String, result: EvaluationResult) = {
    val entry = List[String](dataset, matcher, result.precision.toString, result.recall.toString, result.fmeasure.toString)
    matcher_writer.writeRow(entry);
    matcher_writer.flush
  }

  def log_result(dataset: String, matcher: String, result: EvaluationResult) = {
    val entry = List[String](dataset, matcher, result.precision.toString, result.recall.toString, result.fmeasure.toString)
    result_writer.writeRow(entry);
    result_writer.flush
  }

  def log(message: String) {
    exp_logger.log(Level.DEBUG, message)
  }

}
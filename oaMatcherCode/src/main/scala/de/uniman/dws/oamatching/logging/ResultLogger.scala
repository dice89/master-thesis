package de.uniman.dws.oamatching.logging

import de.unima.dws.omatching.pipeline.EvaluationResult
import com.github.tototoshi.csv.CSVWriter

object ResultLogger {
	
	//logging each value
	val matcher_writer = CSVWriter.open("matching_matcher_result.csv")
	matcher_writer.writeRow(List[String]("dataset", "matcher", "precision","recall","f1-measure"))
	
	
	val result_writer = CSVWriter.open("matching_matcher_result.csv")
	result_writer.writeRow(List[String]("dataset", "matcher", "precision","recall","f1-measure"))
	
	
	def log_matcher_result(dataset:String,matcher:String,result:EvaluationResult)={
	  val entry = List[String](dataset, matcher, result.precision.toString ,result.recall.toString ,result.fmeasure .toString)
	  matcher_writer.writeRow(entry);
	  matcher_writer.flush
	}
		
	def log_result(dataset:String,matcher:String,result:EvaluationResult)={
	  val entry = List[String](dataset, matcher, result.precision.toString ,result.recall.toString ,result.fmeasure .toString)
	  result_writer.writeRow(entry);
	  result_writer.flush
	}
	
}
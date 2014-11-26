package de.unima.dws.omatching.experiments

import java.net.URI
import de.unima.dws.omatching.pipeline.MatcherRegistry
import java.io.BufferedReader
import java.io.FileReader
import java.io.File
import com.github.tototoshi.csv.CSVReader
import de.unima.dws.omatching.pipeline.metaMatcher.AlignmentFromFile
object Start extends App{

  	val onto1:URI = new URI("file:///Users/mueller/Coding/workspace/MasterExperiments/ontos/conference/cmt.owl");
	val onto2:URI = new URI("file:///Users/mueller/Coding/workspace/MasterExperiments/ontos/conference/Conference.owl" );
	
	MatcherRegistry.init
	
	MatcherRegistry.matchAll(onto1, onto2)
  
	
	/*val reader = CSVReader.open(new File("result.csv"));
	reader.allWithHeaders.foreach(tuple => println(tuple.get("Match").get +"-" + tuple.get("outlier").get ))*/
}
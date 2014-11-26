package de.unima.dws.omatching.experiments

import java.net.URI

import de.unima.dws.omatching.pipeline.MatcherRegistry
object Start extends App{

  	val onto1:URI = new URI("file:///Users/mueller/Coding/workspace/MasterExperiments/ontos/conference/cmt.owl");
	val onto2:URI = new URI("file:///Users/mueller/Coding/workspace/MasterExperiments/ontos/conference/Conference.owl" );
	
	MatcherRegistry.init
	
	MatcherRegistry.matchAll(onto1, onto2)
}
package de.unima.dws.omatching.pipeline.metaMatcher

import java.util.Properties
import scala.collection.mutable.{Map => MutableMap}
import org.semanticweb.owl.align.Alignment
import de.unima.dws.omatching.matcher.MatchRelationURI
import scala.collection.mutable.HashMap



class BestSelectOutlierMatchingAlignment(override val matchings: Map[MatchRelationURI, Double], override val threshold:Double)  extends OutlierMatchingCombinationAlignment(matchings,threshold) {

  def align(alignment: Alignment, params: Properties) = {
    
    val best_map:MutableMap[MatchRelationURI, Double] = new HashMap[MatchRelationURI, Double]();
    
    for(match_res<-matchings){
    	if(! best_map.contains(match_res._1)){
    		best_map.+=(match_res)
    	}else {
    		if ( match_res._2  > best_map.get(match_res._1 ).get ){
    		  best_map.+=(match_res)
    		}
    	}
    }
    
    best_map.foreach(A =>{
    	//println(A._2)
      
    	//quick hack to obey owl Thing
    	if(A._2 > threshold && ! ( A._1.left .toString().contains("Thing") || A._1.right .toString().contains("Thing"))){
    		addAlignCell(A._1.left ,A._1.right, A._1.relation , A._2)
    	}
    })
    
    
  }
}
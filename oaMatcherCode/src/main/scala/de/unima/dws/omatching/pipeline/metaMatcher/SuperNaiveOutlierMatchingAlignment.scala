package de.unima.dws.omatching.pipeline.metaMatcher

import java.util.Properties
import org.semanticweb.owl.align.Alignment
import org.semanticweb.owl.align.AlignmentProcess
import fr.inrialpes.exmo.align.impl.URIAlignment
import de.unima.dws.omatching.matcher.MatchRelationURI

/**
 *
 * Simple Transforms and outlier matrix to a matching where the outlier score is the matching score will probably never work this way just for demonstrations purposes
 * @author Alexander C. Mueller
 *
 */
class SuperNaiveOutlierMatchingAlignment(val matchings: Map[MatchRelationURI, Double], val threshold:Double) extends URIAlignment with AlignmentProcess {

  def align(alignment: Alignment, params: Properties) = {
    matchings.foreach(A =>{
    	println(A._2)
    	if(A._2 > threshold){
    		addAlignCell(A._1.left ,A._1.right, A._1.relation , A._2)
    	}
    })
  }
}
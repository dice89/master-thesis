package de.unima.dws.omatching.matcher

import java.net.URI
import org.semanticweb.owl.align.Alignment
import org.semanticweb.owl.align.AlignmentProcess
import fr.inrialpes.exmo.align.impl.DistanceAlignment

case class MatchRelation(left:String,relation:String,right:String)
case class MatchRelationURI(left:URI,right:URI,relation:String)

abstract class BaseMatcher extends DistanceAlignment with AlignmentProcess  {
	protected def align_match(threshold:Double):List[org.semanticweb.owl.align.Cell]
	
	private def alignmentToMap(cells:List[org.semanticweb.owl.align.Cell]):Map[MatchRelation,Double] = {
		
		cells.map(cell => {
			
		  (MatchRelation(cell.getObject1AsURI(this).toString(),cell.getRelation().getRelation(),cell.getObject2AsURI(this).toString()), cell.getStrength())
		 
		} ) toMap
	}
	
	protected def postPrune(threshold:Double)
	
	def align(threshold:Double):Map[MatchRelation,Double] = {
		alignmentToMap(align_match(threshold))
	}
	
	def prepare(onto1:URI,onto2:URI):Unit

}
package de.unima.dws.omatching.matcher

import java.net.URI
import org.semanticweb.owl.align.Alignment
import org.semanticweb.owl.align.AlignmentProcess
import fr.inrialpes.exmo.align.impl.DistanceAlignment
import org.semanticweb.owl.align.Cell
import java.util.ArrayList
import scala.collection.JavaConversions._
import scala.collection.convert.Wrappers.JEnumerationWrapper

case class MatchRelation(left: String, relation: String, right: String)
case class MatchRelationURI(left: URI, right: URI, relation: String)

abstract class BaseMatcher extends DistanceAlignment with AlignmentProcess {
  protected def align_match(threshold: Double): java.util.List[Cell]

  private def alignmentToMap(cells: scala.collection.mutable.Buffer[Cell]): Map[MatchRelation, Double] = {
	
    cells.map(cell => {
      (MatchRelation(cell.getObject1AsURI(this).toString(), cell.getRelation().getRelation(), cell.getObject2AsURI(this).toString()), cell.getStrength())

    }) toMap
  }

   def postPrune(threshold: Double) = {
     val alignments = new JEnumerationWrapper(getElements()).toList;
     
    for(cell<-alignments){
    	if(cell.getStrength()<= threshold) {
    	  this.removeAlignCell(cell);
    	}
    }
    
  }

  def align(threshold: Double): Map[MatchRelation, Double] = {
    val scala_list:scala.collection.mutable.Buffer[Cell] = align_match(threshold) 
     alignmentToMap(scala_list)
  }

  def prepare(onto1: URI, onto2: URI): Unit

}
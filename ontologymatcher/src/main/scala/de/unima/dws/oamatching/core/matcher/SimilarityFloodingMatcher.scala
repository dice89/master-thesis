package de.unima.dws.oamatching.core.matcher

import java.util

import com.interdataworking.mm.alg.{Match, MapPair}
import de.unima.dws.alex.onto2graph.{Matcher, TestMatch}
import de.unima.dws.oamatching.core.{Cell, Alignment}
import org.semanticweb.owlapi.model.OWLOntology
import scala.collection.JavaConversions._

/**
 * Created by mueller on 26/01/15.
 */
class SimilarityFloodingMatcher  extends  StructuralLevelMatcher{

  override protected def align(onto1: OWLOntology, onto2: OWLOntology, initial_Alignment: Alignment, threshold: Double): Alignment = {
    //if no alignment available create initial alignment with jaro Winkler
    val alignment: util.List[MapPair] = if(initial_Alignment==null) TestMatch.createInitialStringMappingJaroWinkler(onto1,onto2)
    else convertAlignmentToMapPair(initial_Alignment)

    val result:Array[MapPair] =  Matcher.structMatch(onto1,onto2,alignment,Match.FORMULA_TFF,Match.FG_PRODUCT)

    convertMapPairToAlignment(result,threshold)
  }

  protected def convertAlignmentToMapPair(alignment:Alignment):java.util.List[MapPair] ={
    val initMap = alignment.correspondences.map(cell => {
      new MapPair(cell.entity1.toString, cell.entity2.toString,cell.measure)
    })

    initMap.toList
  }

  protected def convertMapPairToAlignment(mapPairs:Array[MapPair],threshold:Double):Alignment = {
    val cells = mapPairs.map(pair=> {
      new Cell(pair.getLeft.toString,pair.getRight.toString,pair.sim,"=","TODO")
    }).toList.filter(cell=> cell.measure >= threshold)

    new Alignment(null,null,cells)
  }

}

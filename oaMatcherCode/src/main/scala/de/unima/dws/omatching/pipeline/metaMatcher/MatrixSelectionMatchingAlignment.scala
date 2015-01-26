package de.unima.dws.omatching.pipeline.metaMatcher

import de.unima.dws.omatching.matcher.MatchRelationURI
import java.util.Properties
import org.semanticweb.owl.align.Alignment
import java.net.URI
import fr.inrialpes.exmo.align.impl.BasicRelation


class MatrixSelectionMatchingAlignment(override val matchings: Map[MatchRelationURI, Double], override val threshold: Double) extends OutlierMatchingCombinationAlignment(matchings, threshold) {

  def align(alignment: Alignment, params: Properties) = {

    //get keys as tupeled list
    val match_to_value = matchings.keySet.map(relation => ((relation.left.toString(), relation.right.toString()), matchings.get(relation).get)) toMap

    //get keys as seperate list
    val keys = match_to_value.keySet.unzip

    //get both sets with unique keys as list
    val keyset_left: List[(String, Int)] = keys._1.toSet.toList.zipWithIndex
    val keyset_right: List[(String, Int)] = keys._2.toSet.toList.zipWithIndex

    val element_matrix = keyset_left.map(
      {
        case (left_element, index) =>
          keyset_right.map({ case (right_element, index) => match_to_value.get((left_element, right_element)) }) toArray
      }) toArray

    //get best per row
    val best_index_for_row = element_matrix.map(row =>
      row.zipWithIndex.reduceLeft((B, A) => {
        if (B._1.getOrElse(0.0) > A._1.getOrElse(0.0)) {
          B
        } else {
          A
        }
      }))

    //check if right is used twice, when yes pick the best


    val results = best_index_for_row.zipWithIndex.map({
      case (best_row_elem, index) => {
        val res: Double = best_row_elem._1.getOrElse(0.0)
        		
        val uri_left: URI = new URI(keyset_left(index)._1.trim())
        val uri_right: URI = new URI(keyset_right(best_row_elem._2)._1.trim())
        
        (uri_left, uri_right, "=", best_row_elem._1.getOrElse(0.0))
      }
    })
    //eliminate duplicate right elements
    val best_row_selected = results.groupBy({case(left,right,relation,score) => right} ).map({case(right,list_of_relations) => list_of_relations.reduceLeft((A,B)=>{
      if(A._4 > B._4 ){
        A
      }else {
        B
      }
    })} )
    //select those that are over the threshold and add them to alignment
    best_row_selected.foreach({case(left,right,relation,score) => {
      if(score > threshold && !(left.toString().toLowerCase().contains("thing") || right.toString().toLowerCase().contains("thing") )){
         addAlignCell(left, right, "=", score)
         
          //println(left+ " --" +right + "" +score)
      }
    }})
   /* test.foreach(tuple =>{ 
      if(tuple._4 > threshold){
    	  println(tuple)
    	  addAlignCell(uri_left, uri_right, "=", best_row_elem._1.get)
      }
     })
    

    for ((best_row_elem, index) <- best_index_for_row.zipWithIndex) {
      val res: Double = best_row_elem._1.getOrElse(0.0)
      if (res > threshold && !(keyset_left(index)._1.contains("Thing") || keyset_right(best_row_elem._2)._1.contains("Thing"))) {
        println(keyset_left(index)._1 + " --" + keyset_right(best_row_elem._2)._1 + "" + best_row_elem._1.get + "")
        val uri_left: URI = new URI(keyset_left(index)._1.trim())
        val uri_right: URI = new URI(keyset_right(best_row_elem._2)._1.trim())

        addAlignCell(uri_left, uri_right, "=", best_row_elem._1.get)
      }
    }*/

  }

}
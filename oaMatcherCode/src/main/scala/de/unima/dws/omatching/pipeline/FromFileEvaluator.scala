package de.unima.dws.omatching.pipeline

import java.io.File
import org.apache.log4j.Logger

import scala.collection.convert.Wrappers.JSetWrapper
import scala.collection.convert.Wrappers.JEnumerationWrapper
import de.unima.dws.omatching.outlierdetection.RapidminerBasedOutlierDetection

object FromFileEvaluator {

  def main(arg: Array[String]): Unit = {
    
  /*
    val file: File = new File("output.csv")
    
  	val alignment = Pipeline.combineMatchingsMatrix( RapidminerBasedOutlierDetection.readCSV(file),0.7)  
  	
  	val wrapper = new JEnumerationWrapper(alignment.getElements()).toList
  	
  	for(cell<-wrapper){
  	  println( cell.getObject1().toString() + " " + cell.getRelation().getRelation() +" " + cell.getObject2().toString())
  	}
    val res = Pipeline.validate("ontos/2014/conference/reference-alignment/edas-iasted.rdf")(alignment)

    println(res)
    */
  }
}
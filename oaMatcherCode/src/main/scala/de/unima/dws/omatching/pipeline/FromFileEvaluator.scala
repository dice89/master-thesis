package de.unima.dws.omatching.pipeline

import java.io.File
import scala.collection.convert.Wrappers.JSetWrapper
import scala.collection.convert.Wrappers.JEnumerationWrapper

object FromFileEvaluator {

  def main(arg: Array[String]): Unit = {
    
    

    

    val file: File = new File("result.csv")
    
  	val alignment = Pipeline.combineMatchingsMatrix( Pipeline.readCSV(file),0.45)  
  	
  	val wrapper = new JEnumerationWrapper(alignment.getElements()).toList
  	
  	for(cell<-wrapper){
  	  println( cell.getObject1().toString() + " " + cell.getRelation().getRelation() +" " + cell.getObject2().toString())
  	}
    val res = Pipeline.validate("ontos/2014/conference/reference-alignment/confOf-edas.rdf")(alignment)
    
    println(res)
  }
}
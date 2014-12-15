package de.unima.dws.omatching.pipeline

import java.io.File
import scala.collection.convert.Wrappers.JSetWrapper
import scala.collection.convert.Wrappers.JEnumerationWrapper

object FromFileEvaluator {

  def main(arg: Array[String]): Unit = {
    
    
    val test:String = "ABCsdfsdf";
    
    println(test.endsWith("sdf"))
    val file: File = new File("result.csv")
    
  	val alignment = Pipeline.combineMatchingsMatrix( Pipeline.readCSV(file),0.5)  
  	
  	val wrapper = new JEnumerationWrapper(alignment.getElements()).toList
  	
  	for(cell<-wrapper){
  	  println( cell.getObject1().toString() + " " + cell.getRelation().getRelation() +" " + cell.getObject2().toString())
  	}
    val res = Pipeline.validate("ontos/2014/conference/reference-alignment/cmt-conference.rdf")(alignment)
    
    println(res)
  }
}
package de.unima.dws.omatching.pipeline

import java.io.File

object FromFileEvaluator {

  def main(arg: Array[String]): Unit = {
    val file: File = new File("result.csv")
    
  	val alignment = Pipeline.combineMatchingsBetter( Pipeline.readCSV(file),0.8)  
    
    val res = Pipeline.validate("ontos/2014/conference/reference-alignment/confOf-sigkdd.rdf")(alignment)
    
    println(res)
  }
}
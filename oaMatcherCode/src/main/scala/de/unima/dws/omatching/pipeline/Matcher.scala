package de.unima.dws.omatching.pipeline

import java.net.URI
import java.util.Properties

import scala.collection.convert.Wrappers.JEnumerationWrapper

import org.semanticweb.owl.align.AlignmentProcess

class Matcher(val name:String, var underlying_aligment_process : AlignmentProcess, val properties:Properties ) {
	
	//function to match them
	def o_match(onto1:URI,onto2:URI):scala.collection.immutable.Map[String,Double] = {
	  var return_map = scala.collection.immutable.Map[String,Double]();
	  
	  underlying_aligment_process.init(onto1, onto2);
	  underlying_aligment_process.align(null, properties );
	  
	  val res = new JEnumerationWrapper(underlying_aligment_process.getElements());
	  //Map List to Map[String,Double]
	  res.map(cell =>  cell.getObject1().toString() +" " + cell.getRelation().getRelation() + " " + cell.getObject2().toString() -> cell.getStrength()) toMap;

	  
	}
}

object Matcher{
  def apply(name:String, underlying_aligment_process : AlignmentProcess,properties:Properties){ 
    new Matcher(name,underlying_aligment_process,properties);
  }
}
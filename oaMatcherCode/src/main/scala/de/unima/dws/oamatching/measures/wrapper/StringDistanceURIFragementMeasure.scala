package de.unima.dws.oamatching.measures.wrapper

import fr.inrialpes.exmo.align.impl.MatrixMeasure
import org.semanticweb.owlapi.model.OWLEntity

class StringDistanceURIFragementMeasure(distance_measure: (String,String) => Double) extends MatrixMeasure with MeasureHelper {

	
	def this(similarity:Boolean , distance_measure: (String,String) => Double) {
		this(distance_measure)
		this similarity=similarity;
	}
	
	def measure(cl1:OWLEntity, cl2:OWLEntity ):Double = {
	  
		val frag1:String = getURIFragement(cl1.getIRI().toURI())
		val frag2:String = getURIFragement(cl2.getIRI().toURI())
		
		val measure = distance_measure (frag1,frag2)
		//println(frag1 + " - " + frag2 + " " + measure)
		measure
	}
 
	def classMeasure( cl1:Object, cl2:Object ):Double = {
	   measure(cl1.asInstanceOf[OWLEntity],cl2.asInstanceOf[OWLEntity])
	}
	def propertyMeasure( cl1:Object, cl2:Object ):Double = {
		measure(cl1.asInstanceOf[OWLEntity],cl2.asInstanceOf[OWLEntity])
	}
	def individualMeasure( cl1:Object, cl2:Object ):Double = {
		measure(cl1.asInstanceOf[OWLEntity],cl2.asInstanceOf[OWLEntity])
	}
	
	
}
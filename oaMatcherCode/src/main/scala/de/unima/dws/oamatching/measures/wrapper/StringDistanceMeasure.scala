package de.unima.dws.oamatching.measures.wrapper

import fr.inrialpes.exmo.align.impl.MatrixMeasure
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology
import de.unima.dws.oamatching.measures.MeasureHelper

class StringDistanceMeasure(distance_measure: (String,String) => Double) extends MatrixMeasure with MeasureHelper {

	
	def this(similarity:Boolean , distance_measure: (String,String) => Double) {
		this(distance_measure)
		this similarity=similarity;
	}
	
	def measure(cl1:OWLEntity, cl2:OWLEntity ):Double = {
		val ontology1 =onto1.getOntology().asInstanceOf[OWLOntology]
		val ontology2 =onto1.getOntology().asInstanceOf[OWLOntology]
		
		val frag1:String = getLabel(cl1, ontology1 )
		val frag2:String = getLabel(cl2, ontology2 )
		
		
		val measure = distance_measure (frag1,frag2)
		
		
		//println(cl1.getAnnotations(ontology1))
		if(cl1.isOWLClass()) {
		  //println( cl1.getIRI().toString() + ":::" + cl1.getObjectPropertiesInSignature() +"::::" + cl1.getDataPropertiesInSignature())
		  //println(cl1.asOWLClass().getReferencingAxioms(ontology1))
		  //ontology1.getDataPropertyDomainAxioms(cl1)
		}
		
		if(cl1.isOWLDataProperty()){
		  //println( cl1.getIRI().toString()  + "---" + cl1.getIndividualsInSignature())
		  //println( cl1.getIRI().toString()  + "---" + cl1.getClassesInSignature())
		  //println( cl1.getIRI().toString()  + "---" + cl1.getObjectPropertiesInSignature())
		  //println(cl1.getIRI().toString()  +" ---" + cl1.asOWLDataProperty().getReferencingAxioms(ontology1))
		  //println(onto1.getOntology().asInstanceOf[OWLOntology].getSignature().size())
		}
		
		if (cl1.isOWLObjectProperty()) {
		 ///  println(cl1.getIRI().toString()  +" ---" + cl1.asOWLObjectProperty().getReferencingAxioms(ontology1))
		 
		}
		
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
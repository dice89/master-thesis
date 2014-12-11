package de.unima.dws.oamatching.measures

import fr.inrialpes.exmo.align.impl.MatrixMeasure
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology
import de.unima.dws.oamatching.measures.base.TrainedComponentMatcher
import fr.inrialpes.exmo.ontowrap.LoadedOntology
import com.wcohen.ss.api.StringWrapper
import scala.collection.convert.Wrappers.JSetWrapper


class TrainedMeasure(val baseMatcher: TrainedComponentMatcher) extends MatrixMeasure {

  def this(similarity: Boolean, baseMatcher: TrainedComponentMatcher) {
    this(baseMatcher)
    this similarity = similarity;
  }

   override def initialize(o1: LoadedOntology[Object], o2: LoadedOntology[Object]): Unit = {
    super.initialize(o1, o2)
    
    val ontology1 = onto1.getOntology().asInstanceOf[OWLOntology]
    val ontology2 = onto1.getOntology().asInstanceOf[OWLOntology]

    val class_labels1: List[OWLEntity] = new JSetWrapper(classlist1.keySet()).map(owlclass => owlclass.asInstanceOf[OWLEntity]).toList
    val class_labels2: List[OWLEntity] = new JSetWrapper(classlist2.keySet()).map(owlclass => owlclass.asInstanceOf[OWLEntity]).toList
    val props_labels1: List[OWLEntity] = new JSetWrapper(proplist1.keySet()).map(owlclass => owlclass.asInstanceOf[OWLEntity]).toList
    val props_labels2: List[OWLEntity] = new JSetWrapper(proplist2.keySet()).map(owlclass => owlclass.asInstanceOf[OWLEntity]).toList 
    
    baseMatcher .init(ontology1, ontology2, class_labels1,class_labels2, props_labels1,props_labels2)
  
   }
  
  def measure(cl1: OWLEntity, cl2: OWLEntity): Double = {

    val ontology1 = onto1.getOntology().asInstanceOf[OWLOntology]
    val ontology2 = onto1.getOntology().asInstanceOf[OWLOntology]

    val measure = baseMatcher .score((cl1,ontology1), (cl2,ontology2))

    measure
  }
  
  

  def classMeasure(cl1: Object, cl2: Object): Double = {
    measure(cl1.asInstanceOf[OWLEntity], cl2.asInstanceOf[OWLEntity])
  }
  def propertyMeasure(cl1: Object, cl2: Object): Double = {
    measure(cl1.asInstanceOf[OWLEntity], cl2.asInstanceOf[OWLEntity])
  }
  def individualMeasure(cl1: Object, cl2: Object): Double = {
    measure(cl1.asInstanceOf[OWLEntity], cl2.asInstanceOf[OWLEntity])
  }

}
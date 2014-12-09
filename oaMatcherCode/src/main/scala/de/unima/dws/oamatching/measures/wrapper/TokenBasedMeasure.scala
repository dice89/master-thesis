package de.unima.dws.oamatching.measures.wrapper

import fr.inrialpes.exmo.align.impl.MatrixMeasure
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology
import fr.inrialpes.exmo.ontosim.string.JWNLDistances
import com.wcohen.ss.AbstractStatisticalTokenDistance
import fr.inrialpes.exmo.ontowrap.LoadedOntology
import scala.collection.convert.Wrappers.JSetWrapper
import com.wcohen.ss.BasicStringWrapperIterator
import com.wcohen.ss.api.StringWrapper
import scala.collection.JavaConversions._
import de.unima.dws.oamatching.measures.StringMeasureHelper

class TokenBasedMeasure(preprocess: ((OWLEntity, OWLOntology)) => String, var second_string_SimObject: AbstractStatisticalTokenDistance) extends MatrixMeasure with MeasureHelper {

  def this(similarity: Boolean, preprocess: ((OWLEntity, OWLOntology)) => String, second_string_SimObject: AbstractStatisticalTokenDistance) {
    this(preprocess, second_string_SimObject)
    this similarity = similarity;
  }

  override def initialize(o1: LoadedOntology[Object], o2: LoadedOntology[Object]): Unit = {
    super.initialize(o1, o2)
    
    /*create vocabulary*/
    
    val ontology1: OWLOntology = o1.getOntology().asInstanceOf[OWLOntology]
    val ontology2: OWLOntology = o1.getOntology().asInstanceOf[OWLOntology]
    val class_labels1: List[StringWrapper] = new JSetWrapper(classlist1.keySet()).map(owlclass => second_string_SimObject.prepare(preprocess(owlclass.asInstanceOf[OWLEntity], ontology1))).toList
    val class_labels2: List[StringWrapper] = new JSetWrapper(classlist2.keySet()).map(owlclass => second_string_SimObject.prepare(preprocess(owlclass.asInstanceOf[OWLEntity], ontology2))).toList
    
    val props_labels1: List[StringWrapper] = new JSetWrapper(proplist1.keySet()).map(owlclass => second_string_SimObject.prepare(preprocess(owlclass.asInstanceOf[OWLEntity], ontology1))).toList
    val props_labels2: List[StringWrapper] = new JSetWrapper(proplist2.keySet()).map(owlclass => second_string_SimObject.prepare(preprocess(owlclass.asInstanceOf[OWLEntity], ontology2))).toList
    
    val labels: List[StringWrapper] = class_labels1 ::: class_labels2 ::: props_labels1 ::: props_labels2
  

    val j_iter: java.util.Iterator[StringWrapper] = labels.iterator
    
    second_string_SimObject.train(new BasicStringWrapperIterator(j_iter));

  }

  def measure(cl1: OWLEntity, cl2: OWLEntity): Double = {

    val ontology1 = onto1.getOntology().asInstanceOf[OWLOntology]
    val ontology2 = onto1.getOntology().asInstanceOf[OWLOntology]

    val frag1: String =preprocess(cl1,ontology1)
    val frag2: String = preprocess(cl2,ontology2)
    
    val measure =   second_string_SimObject.score(frag1, frag2)
    if(measure > 0.0){
         // println(frag1 + "--" + frag2 + " =" + measure )
    }

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
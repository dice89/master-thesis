package de.unima.dws.oamatching.measures.wrapper

import java.net.URI
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl
import org.semanticweb.owlapi.model.IRI
import uk.ac.manchester.cs.owl.owlapi.OWLAnnotationPropertyImpl
import org.semanticweb.owlapi.model.OWLAnnotation

trait MeasureHelper {
  def getURIFragement(uri: URI): String = {
    uri.getFragment()
    //TODO check if it is working
  }

  def getLabel(entity: OWLEntity, ontology: OWLOntology): String = {
    var label: String = entity.getIRI().toURI().getFragment()

    if (label == null || label.equals("")) {
      label = entity.getIRI().toString();
    }else{
      return label
    }

    if (label.contains("#")) {
      label = label.substring(label.indexOf('#') + 1);
    }

    if (label.contains("/")) {
      label = label.substring(label.lastIndexOf('/') + 1);
    }

    //check for rdfs label
    val rdf_schema_labels = entity.getAnnotations(ontology, new OWLAnnotationPropertyImpl(
      IRI.create("http://www.w3.org/2000/01/rdf-schema#label")));

    //use rdfs label if existent
    if (rdf_schema_labels != null && rdf_schema_labels.size() > 0) {

      label = rdf_schema_labels.toArray()(0).asInstanceOf[OWLAnnotation].getValue().toString()
      if (label.startsWith("\"")) {
        label = label.substring(1);
      }

      if (label.contains("\"")) {
        label = label.substring(0, label.lastIndexOf('"'));
      }
    }
    
    label
  }

}
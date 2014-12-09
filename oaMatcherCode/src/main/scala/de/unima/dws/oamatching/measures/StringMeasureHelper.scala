package de.unima.dws.oamatching.measures

/**
 *
 * Object that contains all implemented basic string metrics
 * @author Alexander Mueller
 *
 */

import de.unima.dws.oamatching.measures.wrapper._
import fr.inrialpes.exmo.ontosim.string.StringDistances
import org.semanticweb.owlapi.model.OWLAnnotation
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.IRI
import uk.ac.manchester.cs.owl.owlapi.OWLAnnotationPropertyImpl
/**
 *  Util classes for string matching
 *
 * @author Alexander C. Mueller
 *
 */
object StringMeasureHelper {

  /**
   *  Curryable Function to match normalized Strings
   * @param normalization A Normalization Function e.g. to lower case
   * @param matching A Matching function that takes 2 Strings as an Input and then returns a Double value for it
   * @param a
   * @param b
   * @return A double valued string distance
   */
  def distance_normalized(normalization: (String, String) => (String, String))(matching: (String, String) => Double)(a: String, b: String): Double = {
    matching.tupled(normalization(a, b))
  }

  def to_lower_case(a: String, b: String): (String, String) = {
    (a.toLowerCase(), b.toLowerCase())
  }
  
  def distance_lower_cased = distance_normalized(to_lower_case) _


  def tokenize_camel_case(a: String): List[String] = {
    a.split("(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])")toList
  }

  def tokenize_low_dash(a: String): List[String] = {
    a.split("_")toList
  }

  def combine_two_tokenizer(tokenizer_a: String => List[String], tokenizer_b: String => List[String])(a: String): List[String] = {
    tokenizer_a(a).map(token => tokenizer_b(token)).flatten
  }
  
  def token_list_to_String(tokens:List[String]):String= {
       val tokens_string:String = tokens.reduceLeft((A,B) => A +" "+ B)

    tokens_string.trim()
  }


  def getLabel(entity: OWLEntity, ontology: OWLOntology): String = {
    var label: String = entity.getIRI().toURI().getFragment()

    if (label == null || label.equals("")) {
      label = entity.getIRI().toString();
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
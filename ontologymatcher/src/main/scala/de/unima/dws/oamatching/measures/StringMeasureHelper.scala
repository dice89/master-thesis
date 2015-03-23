package de.unima.dws.oamatching.measures


import de.unima.dws.oamatching.config.Config
import org.semanticweb.owlapi.model.{AxiomType, IRI, OWLAnnotation, OWLEntity, OWLLiteral, OWLOntology}
import org.tartarus.snowball.ext.PorterStemmer

import scala.collection.immutable.IndexedSeq
import scala.io.Source

/**
 *  Util classes for string matching
 *
 * @author Alexander C. Mueller
 *
 */
object StringMeasureHelper {

  val stoplist:IndexedSeq[String] = createStopWordList(Config.PATH_TO_STOP_LIST)

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
    (a.toLowerCase.replace("-", ""), b.toLowerCase.replace("-",""))
  }
  
  def to_lower_case_single(a:String):String = {
    a.toLowerCase
  }


  def remove_punctuation(a:String,b:String):(String,String) = {
    (a.replaceAll("[^a-zA-Z ]", "").toLowerCase(),b.replaceAll("[^a-zA-Z ]", "").toLowerCase())
  }

  def distance_lower_cased = distance_normalized(to_lower_case) _

  def distance_ignored_punctuation_lower_cased = distance_normalized(remove_punctuation) _

  def preprocess_porter_stemmed = stem_term(porter_stem)_

  def tokenize_camel_case(a: String): List[String] = {
    a.split("(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])")toList
  }

  def tokenize_low_dash(a: String): List[String] = {
    a.split("_")toList
  }

  def tokenize_low_whiteSpace(a: String): List[String] = {
    a.split(" ")toList
  }

  def combine_two_tokenizer(tokenizer_a: String => List[String], tokenizer_b: String => List[String])(a: String): List[String] = {
    tokenizer_a(a).map(token => tokenizer_b(token)).flatten
  }

  def token_list_to_String(tokens: List[String]): String = {
    val tokens_string: String = tokens.reduceLeft((A, B) => " " + A + " " + B + "")

    tokens_string.trim()
  }

  /**
   * Apply stemming stemming functions
   *
   * @param stem
   * @param a
   * @param b
   * @return
   */
  def stem_term(stem: (String) => String)(a: String, b: String): (String, String) = {

    (stem(a), stem(b))
  }

  /**
   * Porter stemmer call
   * @param a
   * @return
   */
  def porter_stem(a: String): String = {
    val stemmer: PorterStemmer = new PorterStemmer()
    stemmer.setCurrent(a)
    if (stemmer.stem()) {
      stemmer.getCurrent
    }else {
      a
    }
  }

  /**
   * Porter stemmer call
   * @param a
   * @return
   */
  def wordnet_lemmatize(a: String): String = {

    a
  }


  def stemMultiple(terms :List[String]):List[String] = {
      terms.map(term => porter_stem(term))
  }


  def lemmatizeMultiple(terms :List[String]):List[String] = {
      terms.map(term => wordnet_lemmatize(term))
  }

  def stopWordFilter(terms:List[String]):List[String] = {
    val terms_size = terms.size
    val res = terms.filter(term => !stoplist.contains(term.toLowerCase()))
   /* if((terms_size-res.size) >1){
      println("stop filtered something")
    }*/

    res
  }

  /**
   * Stem with basic basic on word net stemmer
   * @param a
   * @return
   */
  def wordnet_stem(a: String): String = {
    //TODO
    //WordNetHelper.getInstance().wnstemmer.StemWordWithWordNet(a)
    a
  }


  /**
   * No preprocessing
   * @param a
   * @return
   */
  def minimalPreprocess(a:String):String = {
    a
  }

  def addPosTag(tokenize_fct:String=>List[String])(a:String):String = {

    val tagger = epic.models.PosTagSelector.loadTagger("en").get // or another 2 letter code.

    val tags = tagger.bestSequence(tokenize_fct(a).toIndexedSeq)

    println(tags)

    a
  }

  def epicTokenizer(a:String):List[String] = {
    null
  }

  def createStopWordList(file:String):IndexedSeq[String] = {
    Source.fromFile(file,"utf-8").getLines.map(stopword => stopword.trim).toIndexedSeq
  }



  /*def getLabel(entity: OWLEntity, ontology: OWLOntology): String = {
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
  }*/
  /*
 def getLabelAndProperties(entity: OWLEntity, ontology: OWLOntology): String = {

   val combined_label: String = getLabel(entity, ontology) +" " +getPropertyLabels(entity, ontology)

   combined_label


 }

 def getPropertyLabels(entity: OWLEntity, ontology: OWLOntology): String = {
   // get Comment


   if (entity.isOWLClass()) {
     val axioms = new JSetWrapper(entity.asOWLClass().getReferencingAxioms(ontology))
     //get axioms
     val labels = for {
       axiom <- axioms;
       signature_elem <- new JSetWrapper(axiom.getSignature());
       if (axiom.getAxiomType().equals(AxiomType.OBJECT_PROPERTY_RANGE) ||
         axiom.getAxiomType().equals(AxiomType.OBJECT_PROPERTY_DOMAIN) ||
         axiom.getAxiomType().equals(AxiomType.DATA_PROPERTY_DOMAIN) ||
         axiom.getAxiomType().equals(AxiomType.DATA_PROPERTY_RANGE))
     } yield {
       if (!signature_elem.equals(entity)) {
         getLabel(signature_elem, ontology)
       } else {
         " "
       }
     }

     (labels.toList mkString " " trim) +" " +  getRDFSComment(entity,ontology)
   } else {
     ""
   }
 }

 def getRDFSComment(entity: OWLEntity, ontology: OWLOntology): String = {

   val annotations = new JSetWrapper(entity.getAnnotations(ontology))

   val comments = for (
     annotation <- annotations;
     if (annotation.getProperty().toString().equals("rdfs:comment"))
   ) yield {
     //println(annotation.getValue().asInstanceOf[OWLLiteral].getLiteral())
     annotation.getValue().asInstanceOf[OWLLiteral].getLiteral().trim()

   }
   //println(comments)
   comments.toList mkString " " trim

 }
 */
}
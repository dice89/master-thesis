package de.unima.dws.omatching.matcher

import java.net.URI
import java.util
import java.util.{Set, Hashtable, Properties}

import fr.inrialpes.exmo.align.impl.{BasicParameters, Extensions, MatrixMeasure}
import fr.inrialpes.exmo.ontowrap.owlapi30.OWLAPI3OntologyFactory
import org.semanticweb.owl.align.{Cell, Alignment}

import scala.collection.convert.Wrappers.JEnumerationWrapper
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions.seqAsJavaList

/**
 * Created by mueller on 21/01/15.
 */
class SimpleIndividualMatcher(val name: String, val similarityObject: MatrixMeasure) extends BaseMatcher {
  protected override def align_match(threshold: Double) = {

    var properties: Properties = new BasicParameters()
    properties.setProperty("threshold", "0.1")
    align(null, properties)
    val asList = new ListBuffer[Cell]
    new JEnumerationWrapper(getElements()).toSeq.copyToBuffer(asList);
    //no unpruning here
    asList
  }

  override def prepare(onto1: URI, onto2: URI): Unit = {
    cleanUp()

    setType("**")

    setSimilarity(similarityObject)

    val ontofactory: OWLAPI3OntologyFactory = new OWLAPI3OntologyFactory()
    val loaded_ontology1 = ontofactory.loadOntology(onto1, false)
    val loaded_ontology2 = ontofactory.loadOntology(onto2, false)

    hash1 = new Hashtable[Object, Set[Cell]]();
    hash2 = new Hashtable[Object, Set[Cell]]();
    extensions = new Extensions();
    namespaces = new Properties();
    init(loaded_ontology1, loaded_ontology2)

  }

  protected override def align(alignment: Alignment, params: Properties) = {

    getSimilarity().initialize(getOntologyObject1(), getOntologyObject2(), alignment);

    getSimilarity().compute(params)

    extract(getType(), params);
  }
}

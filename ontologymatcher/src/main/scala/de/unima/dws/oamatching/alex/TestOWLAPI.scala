package de.unima.dws.alex

import java.io.{InputStream, File}

import com.hp.hpl.jena.rdf.model.{Statement, StmtIterator, ModelFactory, Model}
import com.hp.hpl.jena.util.FileManager
import de.unima.dws.oamatching.core.AlignmentParser
import org.apache.spark.{SparkConf, SparkContext}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLOntologyManager, OWLOntology}

import scala.collection.convert.Wrappers.JSetWrapper

/**
 * Created by mueller on 21/01/15.
 */
object TestOWLAPI extends App {

  val file_aling1: File = new File("cmt-conference.rdf")
  AlignmentParser.parseRDF(file_aling1)
  /*
  val file_onto1: File = new File("cmt.owl");
  val test_iri: IRI = IRI.create(file_onto1);
  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager();

  val ontology: OWLOntology = manager.loadOntology(test_iri);

  if (!ontology.isEmpty) {

    val test = JSetWrapper(ontology.getClassesInSignature())

    val test2 = JSetWrapper(ontology.getSignature());


    for(prop<-test2) {
      println(prop.toStringID)
    }
    println(test.size)
    for (elm <- test) {

      println(elm.getIRI())
      println(elm.getIRI.getNamespace)
      println(elm.getIRI.getShortForm)
    }

  } else {
    println("fail")
  }
*/

 /* val conf = new SparkConf()
    .setAppName("Simple Application")
    //this needs to be parameterized.
    .setMaster("local[1]")
    .set("spark.executor.memory", "1g")

  val sc = new SparkContext(conf)*/




}

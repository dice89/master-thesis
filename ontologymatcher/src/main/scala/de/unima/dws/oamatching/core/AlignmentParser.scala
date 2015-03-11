package de.unima.dws.oamatching.core

import java.io.{File, InputStream}
import java.net.URI

import com.hp.hpl.jena.rdf.model._
import com.hp.hpl.jena.rdf.model.impl.{LiteralImpl, ResourceImpl}
import com.hp.hpl.jena.util.FileManager
import org.semanticweb.owlapi.model.OWLOntology

import scala.collection
import scala.collection.convert.Wrappers.JIteratorWrapper
import scala.collection.immutable.HashSet
import scala.collection.{immutable, mutable}
import scala.xml.Elem
import scala.collection.JavaConversions._

/**
 * Created by mueller on 22/01/15.
 */
object AlignmentParser {
  /**
   * Parsing an RDF based alignment
   * @param path_to_alignment
   * @return
   */
  def parseRDF(path_to_alignment: String): Alignment = {
    val model: Model = ModelFactory.createDefaultModel()
    val in: InputStream = FileManager.get().open(path_to_alignment)

    if (in == null) {
      //TODO ERROR handling
    }

    // read the RDF/XML file
    model.read(in, null)


    val namespace: String = "http://knowledgeweb.semanticweb.org/heterogeneity/alignment"
    val alignment_node: Resource = model.createResource(namespace + "Alignment")

    val iter: StmtIterator = model.listStatements(null, null, alignment_node.asInstanceOf[Resource])

    if (!iter.hasNext) {
      //TODO Error handling
      println("fail")
    }
    val alignment_parent: Resource = iter.nextStatement().getSubject
    //get onto1
    val alignment_onto1_query = model.createProperty(namespace + "onto1")

    val onto1_namespace_prop =  alignment_parent.getProperty(alignment_onto1_query)
    val onto1_namespace =  if(onto1_namespace_prop.isInstanceOf[Literal]){
      alignment_parent.getProperty(alignment_onto1_query).getString
    }else {
      alignment_parent.getProperty(alignment_onto1_query).getResource.getURI.toString
    }

    //get onto2
    val alignment_onto2_query = model.createProperty(namespace + "onto2")


    val onto2_namespace_prop =  alignment_parent.getProperty(alignment_onto2_query)
    val onto2_namespace =  if(onto1_namespace_prop.isInstanceOf[Literal]){
      alignment_parent.getProperty(alignment_onto2_query).getString
    }else {
      alignment_parent.getProperty(alignment_onto2_query).getResource.getURI.toString
    }

    val alignment_cell_query = model.createProperty(namespace + "map")
    //wrap and map to RDFResource
    val alignment_cells: List[RDFNode] = JIteratorWrapper(alignment_parent.listProperties(alignment_cell_query)).toList.map(cell => cell.getObject)


    //map to cells
    val correspondences = alignment_cells.map(cell => {
      if(cell.isResource){
           val test = cell.asInstanceOf[ResourceImpl]
      val relation: String = test.getProperty(model.createProperty(namespace + "relation")).getString

      val measure: Double = test.getProperty(model.createProperty(namespace + "measure")).getLiteral.getLexicalForm.toDouble

      val entity1: URI = new URI(test.getProperty(model.createProperty(namespace + "entity1")).getResource.getURI)
      val entity2: URI = new URI(test.getProperty(model.createProperty(namespace + "entity2")).getResource.getURI)

      Option(MatchingCell(entity1.toString, entity2.toString, measure, relation, Cell.TYPE_UNKOWN))


      }else {
        //means that in the alignment is an empty mapping in the form
        // <map>
        //
        // </map>
        // so ignore it
        Option.empty
      }
    }).toList


    val cleaned_correspondences = correspondences.filter(_.isDefined).map(_.get)

    new Alignment(onto1_namespace, onto2_namespace, cleaned_correspondences)
  }

  /**
   * Add the relation owl type to the ontologies
   * @param path_to_alignment
   * @param path_to_onto1
   * @param path_to_onto2
   * @return
   */
  def parseRDFWithOntos(path_to_alignment: String, path_to_onto1:String, path_to_onto2:String): Alignment = {
    val model: Model = ModelFactory.createDefaultModel()
    val in: InputStream = FileManager.get().open(path_to_alignment)

    if (in == null) {
      //TODO ERROR handling
    }

    //parse ontos and make the IRIs of the classes and properties random access available
    val onto1: OWLOntology = OntologyLoader.load(path_to_onto1)
    val onto2: OWLOntology = OntologyLoader.load(path_to_onto2)

    val onto1_obj_properties:Vector[String] = onto1.getObjectPropertiesInSignature().toVector.map(property => property.getIRI.toString).toVector
    val onto2_obj_properties:Vector[String] = onto2.getObjectPropertiesInSignature().toVector.map(property => property.getIRI.toString).toVector

    val onto1_data_properties:Vector[String] = onto1.getDataPropertiesInSignature().toVector.map(property => property.getIRI.toString).toVector
    val onto2_data_properties:Vector[String] =  onto2.getDataPropertiesInSignature().toVector.map(property => property.getIRI.toString).toVector

    val onto1_classes:Vector[String] = onto1.getClassesInSignature().map(o_class => o_class.getIRI.toString).toVector
    val onto2_classes:Vector[String] =  onto2.getClassesInSignature().map(o_class => o_class.getIRI.toString).toVector


    // read the RDF/XML file
    model.read(in, null)


    val namespace: String = "http://knowledgeweb.semanticweb.org/heterogeneity/alignment"
    val alignment_node: Resource = model.createResource(namespace + "Alignment")

    val iter: StmtIterator = model.listStatements(null, null, alignment_node.asInstanceOf[Resource])

    if (!iter.hasNext) {
      //TODO Error handling
      println("fail")
    }
    val alignment_parent: Resource = iter.nextStatement().getSubject


    val alignment_onto1_uri_query = model.createProperty(namespace + "uri1")
    val onto1_uri: String = getOntoProperty(alignment_parent, alignment_onto1_uri_query)

    //get onto1
    val alignment_onto1_query = model.createProperty(namespace + "onto1")
    val onto1_namespace: String = if(onto1_uri.equals("nn")) {
      getOntoProperty(alignment_parent, alignment_onto1_query)
    }else {
      onto1_uri
    }


    val alignment_onto2_uri_query = model.createProperty(namespace + "uri2")
    val onto2_uri: String = getOntoProperty(alignment_parent, alignment_onto2_uri_query)

    //get onto2
    val alignment_onto2_query = model.createProperty(namespace + "onto2")
    val onto2_namespace: String = if(onto2_uri.equals("nn")) {
      getOntoProperty(alignment_parent, alignment_onto2_query)
    }else {
      onto2_uri
    }

    val alignment_cell_query = model.createProperty(namespace + "map")
    //wrap and map to RDFResource
    val alignment_cells: List[RDFNode] = JIteratorWrapper(alignment_parent.listProperties(alignment_cell_query)).toList.map(cell => cell.getObject)




    val correspondences = alignment_cells.map(cell => {
      if(cell.isResource){
        val cell_casted = cell.asInstanceOf[ResourceImpl]
        val relation: String = cell_casted.getProperty(model.createProperty(namespace + "relation")).getString

        val measure: Double = cell_casted.getProperty(model.createProperty(namespace + "measure")).getLiteral.getLexicalForm.toDouble

        val entity1: URI = new URI(cell_casted.getProperty(model.createProperty(namespace + "entity1")).getResource.getURI)
        val entity2: URI = new URI(cell_casted.getProperty(model.createProperty(namespace + "entity2")).getResource.getURI)

        // add type of relation with ontos
        val cell_type = if(onto1_classes.contains(entity1.toString) && onto2_classes.contains(entity2.toString)){
          Cell.TYPE_CLASS
        }else if(onto1_obj_properties.contains(entity1.toString) && onto2_obj_properties.contains(entity2.toString))  {
          Cell.TYPE_OBJECT_PROPERTY
        }else if(onto1_data_properties.contains(entity1.toString) && onto2_data_properties.contains(entity2.toString))  {
          Cell.TYPE_OBJECT_PROPERTY
        }else {
          // individuals

          Cell.TYPE_UNKOWN
        }

        Option(MatchingCell(entity1.toString, entity2.toString, measure, relation, cell_type))
      }else {
        //means that in the alignment is an empty mapping in the form
        // <map>
        //
        // </map>
        // so ignore it
        Option.empty
      }
    }).toList


    val cleaned_correspondences = correspondences.filter(_.isDefined).map(_.get)


    new Alignment(onto1_namespace, onto2_namespace, cleaned_correspondences)
  }

  def getOntoProperty(alignment_parent: Resource, alignment_onto2_query: Property): String = {
    val onto2_namespace_prop = alignment_parent.getProperty(alignment_onto2_query)
    val onto2_namespace = if (alignment_parent.hasProperty(alignment_onto2_query)) {

      try {
        alignment_parent.getProperty(alignment_onto2_query).getResource.getURI.toString
      } catch {
        case _: Throwable => alignment_parent.getProperty(alignment_onto2_query).getString
      }
    } else {
      "nn"
    }
    onto2_namespace
  }

  /**
   * Parsing an RDF based alignment
   * @param alignment_file
   * @return
   */
  def parseRDF(alignment_file: File): Alignment = {
    parseRDF(alignment_file.toURI.toString)
  }


  def writeRDF(alignment: Alignment, file: String): Unit = {

    def getCell(id: Int, entity1: String, entity2: String, measure: Double, relation: String): Elem = {
      <map>
        <Cell cid={id+""}>
          <entity1 rdf:resource={entity1}/>
          <entity2 rdf:resource={entity2}/>
          <measure rdf:datatype='xsd:float'>
            {measure}
          </measure>
          <relation>
            {relation}
          </relation>
        </Cell>
      </map>



    }

    def getRDFDoc(content: Elem): Elem = <rdf:RDF xmlns='http://knowledgeweb.semanticweb.org/heterogeneity/alignment'
                                                  xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'
                                                  xmlns:xsd='http://www.w3.org/2001/XMLSchema#'>
      {content}
    </rdf:RDF>

    def getAlignment(onto1: Elem, onto2: Elem, cells:  immutable.Seq[Elem]): Elem = {
      <Alignment>
        <xml>yes</xml>
        <level>0</level>
        <type>??</type>
        <onto1>
          {onto1}
        </onto1>
        <onto2>
          {onto2}
        </onto2>
        {cells}
      </Alignment>
    }

    def getOnto(about: String, location: String): Elem =
          <Ontology rdf:about={about}>
           <location>
            {location}
            </location>
          </Ontology>




    val cells = alignment.correspondences.toList.zipWithIndex.map { case (cell, index) =>
      getCell(index + 1, cell.entity1.toString, cell.entity2.toString, cell.measure, cell.relation)
    }
    val xml_cells: immutable.Seq[Elem] = collection.immutable.Seq(cells: _*)

    val onto1 = getOnto(alignment.onto1,"")

    val xml_onto1: Elem = getOnto(alignment.onto1,"")
    val xml_onto2: Elem = getOnto(alignment.onto2,"")


    val xml_alignment = getAlignment(xml_onto1,xml_onto2,xml_cells)

    val rdf_doc = getRDFDoc(xml_alignment)
    scala.xml.XML.save(file,rdf_doc,"UTF-8", true, null)

  }
}

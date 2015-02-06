package de.unima.dws.oamatching.core

import java.io.{File, InputStream}
import java.net.URI

import com.hp.hpl.jena.rdf.model._
import com.hp.hpl.jena.util.FileManager

import scala.collection
import scala.collection.convert.Wrappers.JIteratorWrapper
import scala.collection.{immutable, mutable}
import scala.xml.Elem

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
    val onto1_namespace = alignment_parent.getProperty(alignment_onto1_query).getResource.getURI

    //get onto2
    val alignment_onto2_query = model.createProperty(namespace + "onto2")
    val onto2_namespace = alignment_parent.getProperty(alignment_onto2_query).getResource.getURI


    val alignment_cell_query = model.createProperty(namespace + "map")
    //wrap and map to RDFResource
    val alignment_cells: List[Resource] = JIteratorWrapper(alignment_parent.listProperties(alignment_cell_query)).toList.map(cell => cell.getResource)


    //map to cells
    val correspondences = alignment_cells.map(cell => {
      val relation: String = cell.getProperty(model.createProperty(namespace + "relation")).getString

      val measure: Double = cell.getProperty(model.createProperty(namespace + "measure")).getLiteral.getLexicalForm.toDouble

      val entity1: URI = new URI(cell.getProperty(model.createProperty(namespace + "entity1")).getResource.getURI)
      val entity2: URI = new URI(cell.getProperty(model.createProperty(namespace + "entity2")).getResource.getURI)

      new Cell(entity1, entity2, measure, relation, Cell.TYPE_UNKOWN)
    }).toList


    new Alignment(onto1_namespace, onto2_namespace, correspondences)
  }

  /**
   * Parsing an RDF based alignment
   * @param alignment_file
   * @return
   */
  def parseRDF(alignment_file: File): Alignment = {
    println(alignment_file.toURI.toString)
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

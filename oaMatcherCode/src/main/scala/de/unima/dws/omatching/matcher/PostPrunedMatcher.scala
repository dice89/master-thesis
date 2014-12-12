package de.unima.dws.omatching.matcher

import java.io.File
import java.net.URI
import java.util.Properties
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.convert.Wrappers.JEnumerationWrapper
import org.semanticweb.owl.align.Alignment
import de.unima.dws.oamatching.measures.SimpleMeasures
import de.unima.dws.oamatching.measures.StandardMeasure
import de.unima.dws.oamatching.measures.StringMeasureHelper
import de.unima.dws.oamatching.measures.base.StringFunctionMatcher
import fr.inrialpes.exmo.align.impl.BasicParameters
import fr.inrialpes.exmo.align.impl.MatrixMeasure
import fr.inrialpes.exmo.ontowrap.owlapi30.OWLAPI3OntologyFactory
import scala.collection.mutable.Buffer
import org.semanticweb.owl.align.Cell
import scala.collection.mutable.ListBuffer

class PostPrunedMatcher(val name: String, val similarityObject: MatrixMeasure) extends BaseMatcher {

  override def prepare(onto1: URI, onto2: URI) = {
    cleanUp()

    setType("**")

    setSimilarity(similarityObject)

    val ontofactory: OWLAPI3OntologyFactory = new OWLAPI3OntologyFactory()

    val loaded_ontology1 = ontofactory.loadOntology(onto1, false)
    val loaded_ontology2 = ontofactory.loadOntology(onto2, false)
    init(loaded_ontology1, loaded_ontology2)

  }

  protected override def align(alignment: Alignment, params: Properties) = {
    getSimilarity().initialize(getOntologyObject1(), getOntologyObject2(), alignment);
    getSimilarity().compute(params)
    
    extract("**", params);
  }

  protected override def align_match(threshold: Double) = {

    var properties: Properties = new BasicParameters()
    properties.setProperty("threshold", "0.1")
    align(null, properties)
    val test = new ListBuffer[Cell]
    new JEnumerationWrapper(getElements()).toSeq.copyToBuffer(test);
    //Due to the fact that we want to have both an unpruned vector in the Matrix and threshold pruned results for the base evaluation, we need to implement the pruning on our own as post-pruning
    postPrune(threshold)
    //Here has to come in some way a threshold optimization
    test
  }

  /*Do it this weird way because the removeAlignCell is implemented by object similarity
 */

}

object PostPrunedMatcher {

  def main(args: Array[String]) {

    val file_onto1: File = new File("ontos/2014/conference/edas.owl");

    val file_onto2: File = new File("ontos/2014/conference/iasted.owl");

    val file_onto3: File = new File("ontos/2014/conference/confOf.owl");

    //val gs_align: String = "ontos/2014/conference/reference-alignment/cmt-conference.rdf"

    val onto1: URI = file_onto1.toURI()
    val onto2: URI = file_onto2.toURI()
    val onto3: URI = file_onto3.toURI()

    //tokenization functions, maybe add stemming
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _
    val simple_preprocessing = Function.untupled(tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem _ compose (StringMeasureHelper.getLabelAndProperties _).tupled)

    // def test3 = StringMeasureHelper.getLabel.andThen test2 

    val measure = new StandardMeasure(false, new StringFunctionMatcher(simple_preprocessing, SimpleMeasures.computeJaccard))

    val test = new PostPrunedMatcher("test", measure)
    
    test.prepare(onto1, onto2)
    println(test.align(0.1).size)

    //println(StringDistances.hammingDistance("test", "test123"))
    /*  println(JEnumerationWrapper(test.getElements()).toList.size)
    
    test.prepare(onto2,onto3)
    
    println(test.align(0.5).size)
    
    println(JEnumerationWrapper(test.getElements()).toList.size))*/

  }
}

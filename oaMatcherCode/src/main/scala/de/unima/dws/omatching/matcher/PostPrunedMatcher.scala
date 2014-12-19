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
import de.unima.dws.omatching.pipeline.EvaluationResult
import fr.inrialpes.exmo.align.impl.eval.PRecEvaluator
import fr.inrialpes.exmo.align.impl.Extensions
import java.util.Hashtable
import java.util.Set
import de.unima.dws.omatching.pipeline.MatchingProblem

class PostPrunedMatcher(val name: String, val similarityObject: MatrixMeasure, var reference: Alignment) extends BaseMatcher {

  def this(name: String, similarityObject: MatrixMeasure) = {
    this(name, similarityObject, null)
  }

  def prepare(problem:MatchingProblem):Unit ={
    prepare(problem.ontology1 , problem.ontology2 ,problem.reference )
  }
  
  def prepare(onto1: URI, onto2: URI, reference: Alignment): Unit = {
    this.reference = reference
    prepare(onto1, onto2);
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

  protected override def align_match(threshold: Double) = {

    var properties: Properties = new BasicParameters()
    properties.setProperty("threshold", "0.1")
    align(null, properties)
    val asList = new ListBuffer[Cell]
    new JEnumerationWrapper(getElements()).toSeq.copyToBuffer(asList);
    //Due to the fact that we want to have both an unpruned vector in the Matrix and threshold pruned results for the base evaluation, we need to implement the pruning on our own as post-pruning
    postPrune(threshold)
    //Here has to come in some way a threshold optimization
    asList
  }

  def align_optimize_only(threshold: Double): Unit = {
    var properties: Properties = new BasicParameters()
    properties.setProperty("threshold", threshold + "")
    setType(getType())

    val start = System.currentTimeMillis()
    align(null, properties)
    val total = System.currentTimeMillis() - start

    println(total)
  }

  def evaluate(): EvaluationResult = {

    val p_rec_eval = new PRecEvaluator(reference, this);
    val alignments = new JEnumerationWrapper(reference.getElements()).toList;
    //println("REFERENCE ALIGNMENT SIZE" + alignments.size)

    p_rec_eval.eval(null)

   
    val res = EvaluationResult(p_rec_eval.getPrecision(), p_rec_eval.getRecall(), p_rec_eval.getFmeasure(), p_rec_eval.getCorrect(), p_rec_eval.getFound(),p_rec_eval.getExpected())

    res
  }

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

    val test = new PostPrunedMatcher("test", measure, null)

    test.prepare(onto1, onto2)
    println(test.align(0.1).size)

    //println(StringDistances.hammingDistance("test", "test123"))
    /*  println(JEnumerationWrapper(test.getElements()).toList.size)
    
    test.prepare(onto2,onto3)
    
    println(test.align(0.5).size)
    
    println(JEnumerationWrapper(test.getElements()).toList.size))*/

  }
}

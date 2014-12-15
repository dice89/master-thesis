package de.unima.dws.omatching.matcher

import java.io.File
import java.net.URI
import java.util.Properties
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.convert.Wrappers.JEnumerationWrapper
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MutableList
import org.semanticweb.owl.align.Alignment
import org.semanticweb.owl.align.Cell
import de.unima.dws.oamatching.measures.StandardMeasure
import de.unima.dws.oamatching.measures.StringMeasureHelper
import de.unima.dws.oamatching.measures.base.StringFunctionMatcher
import de.unima.dws.omatching.pipeline.EvaluationResult
import fr.inrialpes.exmo.align.impl.BasicParameters
import fr.inrialpes.exmo.align.impl.MatrixMeasure
import fr.inrialpes.exmo.align.impl.eval.PRecEvaluator
import fr.inrialpes.exmo.align.parser.AlignmentParser
import fr.inrialpes.exmo.ontosim.string.StringDistances
import de.unima.dws.omatching.pipeline.EvaluationResult

class GridOptmizeMatcher(override val name: String, override val similarityObject: MatrixMeasure, val gridSize: Int) extends PostPrunedMatcher(name, similarityObject,null) {

  protected override def align_match(threshold: Double) = {

    var properties: Properties = new BasicParameters()
    properties.setProperty("threshold", "0.1")
    align(null, properties)
    val asList = new ListBuffer[Cell]
    new JEnumerationWrapper(getElements()).toSeq.copyToBuffer(asList);
    //Due to the fact that we want to have both an unpruned vector in the Matrix and threshold pruned results for the base evaluation, we need to implement the pruning on our own as post-pruning

    //postPrune(threshold)
  	gridoptimize()
    //println(gridoptimize())
    //Here has to come in some way a threshold optimization
    asList
  }

  private def gridoptimize(): (Double, EvaluationResult) = {
    val alignments = new JEnumerationWrapper(getElements()).toList;

    val optimizationresults: MutableList[(Double, EvaluationResult)] = new MutableList()

    //TODO make funcitonal
    val stepsize = 1.0 / this.gridSize;
    var curr_threshold = 0.0;
    while (curr_threshold <= 1.0) {
      optimizationresults += optimization_round(curr_threshold);
      curr_threshold = curr_threshold + stepsize
    }

    optimizationresults.reduceLeft((A, B) => {
      if (A._2.fmeasure > B._2.fmeasure) {
        A
      } else {
        B
      }
    })

  }

  private def optimization_round(threshold: Double): (Double, EvaluationResult) = {
    val alignments = new JEnumerationWrapper(getElements()).toList;
    val removed_cells: MutableList[Cell] = new MutableList[Cell]();
    alignments.map(cell => {
      var removed_cell: Cell = null
      if (cell.getStrength() < threshold) {
        removed_cells += cell
        this.removeAlignCell(cell);
      }
    })
    //println(removed_cells.size)

    //now evaluate and return the result including the given threshold
    (threshold, evaluate())

  }

}

object GridOptmizeMatcher {

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

    val measure = new StandardMeasure(false, new StringFunctionMatcher(StringMeasureHelper.getLabel, StringDistances.smoaDistance))

    val gs_align: String = "ontos/2014/conference/reference-alignment/edas-iasted.rdf"
    var aparser: AlignmentParser = new AlignmentParser(0);
    var reference: Alignment = aparser.parse(new File(gs_align).toURI());

    val test = new GridOptmizeMatcher("test", measure, 40)

    test.prepare(onto1, onto2, reference)
    println(test.align(0.1).size)

    //println(StringDistances.hammingDistance("test", "test123"))
    /*  println(JEnumerationWrapper(test.getElements()).toList.size)
    
    test.prepare(onto2,onto3)
    
    println(test.align(0.5).size)
    
    println(JEnumerationWrapper(test.getElements()).toList.size))*/

  }
}
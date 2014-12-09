package de.unima.dws.omatching.matcher

import java.io.File
import java.net.URI
import java.util.Properties
import scala.collection.convert.Wrappers.JEnumerationWrapper
import org.semanticweb.owl.align.Alignment
import org.semanticweb.owl.align.Cell
import com.wcohen.ss.TFIDF
import de.unima.dws.oamatching.measures.StringMeasureHelper
import de.unima.dws.oamatching.measures.wrapper.TokenBasedMeasure
import fr.inrialpes.exmo.align.impl.BasicParameters
import fr.inrialpes.exmo.align.impl.MatrixMeasure
import com.wcohen.ss.tokens.SimpleTokenizer
import de.unima.dws.oamatching.measures.wrapper.StringDistanceMeasure
import fr.inrialpes.exmo.ontosim.string.StringDistances

class PostPrunedMatcher(val name: String, val similarityObject: MatrixMeasure) extends BaseMatcher {

  override def prepare(onto1: URI, onto2: URI) = {
    cleanUp()

    setType("?*")

    setSimilarity(similarityObject)
    init(onto1, onto2)

  }

  protected override def align(alignment: Alignment, params: Properties) = {
    getSimilarity().initialize(getOntologyObject1(), getOntologyObject2(), alignment);
    getSimilarity().compute(params)

    extract("?*", params);
  }

  protected override def align_match(threshold: Double) = {

    var properties: Properties = new BasicParameters()
    align(null, properties)

    val return_val = new JEnumerationWrapper(getElements()).toList;
    //Due to the fact that we want to have both an unpruned vector in the Matrix and threshold pruned results for the base evaluation, we need to implement the pruning on our own as post-pruning
    postPrune(threshold)

    return_val
  }

  /*Do it this weird way because the removeAlignCell is implemented by object similarity
 */
  protected override def postPrune(threshold: Double) = {
    val enumerator = getElements()
    while (enumerator.hasMoreElements()) {
      val cell: Cell = enumerator.nextElement()
      if (cell.getStrength() <= threshold) {
        this.removeAlignCell(cell)
      }
    }
  }
}

object PostPrunedMatcher {

  def main(args: Array[String]) {

    val file_onto1: File = new File("ontos/2014/conference/MICRO.owl");

    val file_onto2: File = new File("ontos/2014/conference/Conference.owl");

    val file_onto3: File = new File("ontos/2014/conference/confOf.owl");

    //val gs_align: String = "ontos/2014/conference/reference-alignment/cmt-conference.rdf"

    val onto1: URI = file_onto1.toURI()
    val onto2: URI = file_onto2.toURI()
    val onto3: URI = file_onto3.toURI()
    
    //tokenization functions, maybe add stemming
    val tokenizer = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _
    val tokens_to_string = StringMeasureHelper.token_list_to_String _

    // def test3 = StringMeasureHelper.getLabel.andThen test2 
    val test = new PostPrunedMatcher("test", new StringDistanceMeasure(StringMeasureHelper.distance_lower_cased(StringDistances.hammingDistance)))

    test.prepare(onto1, onto2)
    println(test.align(0.5).size)

    
    //println(StringDistances.hammingDistance("test", "test123"))
    /*  println(JEnumerationWrapper(test.getElements()).toList.size)
    
    test.prepare(onto2,onto3)
    
    println(test.align(0.5).size)
    
    println(JEnumerationWrapper(test.getElements()).toList.size)*/

  }
}

package de.unima.dws.omatching.matcher

import java.io.File
import java.net.URI
import java.util.Properties
import scala.collection.convert.Wrappers.JEnumerationWrapper
import org.semanticweb.owl.align.Alignment
import org.semanticweb.owl.align.AlignmentProcess
import de.unima.dws.oamatching.measures.StringMeasureHelper
import de.unima.dws.oamatching.measures.wrapper.StringDistanceURIFragementMeasure
import fr.inrialpes.exmo.align.impl.BasicParameters
import fr.inrialpes.exmo.align.impl.DistanceAlignment
import fr.inrialpes.exmo.ontosim.string.StringDistances
import fr.inrialpes.exmo.align.impl.Similarity
import org.semanticweb.owl.align.Cell

class URIFragmentStringMatcher(val name: String, sim_fct: (String,String)=> Double) extends BaseMatcher  {

  override def prepare(onto1:URI,onto2:URI) = {
    cleanUp()
    setSimilarity(new StringDistanceURIFragementMeasure(false,sim_fct))
    setType("?*")
    init(onto1,onto2)
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
    
    println(return_val.size)
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

object URIFragmentStringMatcher {
  def apply(name: String, sim_fct: (String,String)=> Double) = {
    var matcher: URIFragmentStringMatcher = new URIFragmentStringMatcher("", StringMeasureHelper.distance_lower_cased(StringDistances.ngramDistance));
    matcher.setType("?*")
    matcher
  }

  def main(args: Array[String]) {

    val file_onto1: File = new File("ontos/2014/conference/cmt.owl");

    val file_onto2: File = new File("ontos/2014/conference/Conference.owl");
    
    val file_onto3: File = new File("ontos/2014/conference/confOf.owl");

    //val gs_align: String = "ontos/2014/conference/reference-alignment/cmt-conference.rdf"

    val onto1: URI = file_onto1.toURI()
    val onto2: URI = file_onto2.toURI()
    val onto3: URI = file_onto3.toURI()
    
    val test = URIFragmentStringMatcher("test",StringMeasureHelper.distance_lower_cased(StringDistances.ngramDistance))
    
    
    test.prepare(onto1, onto2)
    println(test.align(0.5).size)
    
    println(JEnumerationWrapper(test.getElements()).toList.size)
    
    test.prepare(onto2,onto3)
    
    println(test.align(0.5).size)
    
    println(JEnumerationWrapper(test.getElements()).toList.size)
    
  }
}

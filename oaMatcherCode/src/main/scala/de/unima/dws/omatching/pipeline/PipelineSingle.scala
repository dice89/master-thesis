package de.unima.dws.omatching.pipeline

import java.io.File
import java.net.URI
import de.unima.dws.omatching.outlierdetection.RapidminerBasedOutlierDetection
import fr.inrialpes.exmo.align.parser.AlignmentParser
import org.semanticweb.owl.align.Alignment

object PipelineSingle {
  def main(args: Array[String]): Unit = {

   /* val ds_name = "edas-iasted"
    val file_onto1: File = new File("ontos/2014/conference/edas.owl");

    val file_onto2: File = new File("ontos/2014/conference/iasted.owl");

    val gs_align: String = "ontos/2014/conference/reference-alignment/"+ds_name+".rdf"

    val onto1: URI = file_onto1.toURI()
    val onto2: URI = file_onto2.toURI()

    var aparser: AlignmentParser = new AlignmentParser(0);
    var reference: Alignment = aparser.parse(new File(gs_align).toURI());

    val problem: MatchingProblem = MatchingProblem(onto1, onto2, reference, ds_name)

    val res = Pipeline.metaMatchAndEvaluate(RapidminerBasedOutlierDetection.rapidminerOutlierReadWrite(ds_name))(Pipeline.combineMatchingsMatrix)(Pipeline.validate(problem.reference))(problem, ds_name, 0.9)

    println(res.metaResult)*/
  }
}
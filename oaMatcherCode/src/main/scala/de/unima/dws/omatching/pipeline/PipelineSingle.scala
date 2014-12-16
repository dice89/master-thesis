package de.unima.dws.omatching.pipeline

import java.io.File
import java.net.URI

object PipelineSingle {
  def main(args: Array[String]): Unit = {
    val file_onto1: File = new File("ontos/2014/conference/conference.owl");

    val file_onto2: File = new File("ontos/2014/conference/edas.owl");

    val gs_align: String = "ontos/2014/conference/reference-alignment/conference-edas.rdf"

    val onto1: URI = file_onto1.toURI()
    val onto2: URI = file_onto2.toURI()

    val res = Pipeline.match_and_evaluate(Pipeline.rapidminerTest(Pipeline.writeCSV("test"))(Pipeline.readCSV))(Pipeline.combineMatchingsBetter)(Pipeline.validate(gs_align))(onto1, onto2, gs_align,0.8)

  }
}
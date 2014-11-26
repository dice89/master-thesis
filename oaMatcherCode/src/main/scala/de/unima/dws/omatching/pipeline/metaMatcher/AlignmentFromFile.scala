package de.unima.dws.omatching.pipeline.metaMatcher

import org.semanticweb.owl.align.AlignmentProcess
import fr.inrialpes.exmo.align.impl.URIAlignment
import java.util.Properties
import org.semanticweb.owl.align.Alignment
import com.github.tototoshi.csv.CSVReader
import java.io.File
import java.net.URI

class AlignmentFromFile(val file: String) extends URIAlignment with AlignmentProcess {

  def align(alignment: Alignment, params: Properties) = {
    val reader = CSVReader.open(new File("result.csv"));

    val mapped_values = reader.allWithHeaders.map(tuple =>
      {
        val matchString: String = tuple.get("Match").get

        val uri1: String = matchString.splitAt(matchString.indexOf(">"))._1.replace("<", "")

        val rest: String = matchString.drop(matchString.indexOf(">"));

        val operator: String = rest.substring(2, 4).trim()

        val uri2: String = rest.splitAt(rest.indexOf("<") + 1)._2.replace(">", "")

        println(uri1)
        println(uri2)
        println(operator.trim())

        (new URI(uri1), new URI(uri2), operator) -> tuple.get("outlier").get.toDouble
      }) toMap

    val finalmap =  mapped_values.map(A => A._1 -> A._2 /  mapped_values.values.max)
    finalmap.foreach(A => 
      addAlignCell(A._1 ._1 , A._1 ._2,  A._1 ._3,A._2 )
    )

  }
}
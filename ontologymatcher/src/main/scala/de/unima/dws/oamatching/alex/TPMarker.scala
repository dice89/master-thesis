package de.unima.dws.oamatching.alex

import java.io.File

import com.github.tototoshi.csv.{CSVWriter, CSVReader}
import de.unima.dws.oamatching.core.AlignmentParser

/**
 * Created by mueller on 23/04/15.
 */
object TPMarker extends App{


  //read input vector read
    val alignment = AlignmentParser.parseRDF("ontos/2014/conference/reference-alignment/conference-confOf.rdf")

  //read reference alignment

  val path_to_matching = "matchings/conference/matchings/conference-confOf_raw_matchings.csv"

  val normal_vector_result_file = new File(path_to_matching)


  val writer = CSVWriter.open("tp_marked.csv")

  //read and write header
  val reader_header= CSVReader.open(normal_vector_result_file)
  val header_line = reader_header.readNext().get ++ List("true_positive")
  writer.writeRow(header_line )
  reader_header.close()

  val reader_normal = CSVReader.open(normal_vector_result_file)

  reader_normal.all().tail.map(line => {

    val left = line(0)
    val right = line(2)
    val tp = if(alignment.containsCorrespondence(left, right)){
      true
    }else {
      false
    }

    val row  = line ++ List(tp+"")

    writer.writeRow(row.toSeq)
  })

  writer.close()

  //match both
}

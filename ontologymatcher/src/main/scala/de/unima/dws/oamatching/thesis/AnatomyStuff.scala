package de.unima.dws.oamatching.thesis

import com.github.tototoshi.csv.CSVReader
import de.unima.dws.oamatching.core.Alignment

/**
 * Created by mueller on 29/04/15.
 */
object AnatomyStuff extends App {
  val it = CSVReader.open("mouse-human_raw_matchings.csv").iterator


  val header_line = it.next()
  val owl_type_index = header_line.indexOf("owl_type")
  val match_type_index = header_line.indexOf("match_type")
  println(match_type_index)
  val left_index = header_line.indexOf("left")
  val right_index = header_line.indexOf("right")
  val relation_index = header_line.indexOf("relation")
  var line = Seq("")
  var class_size = 0;
  var dp_size = 0;
  var op_size = 0;

  val startime = System.currentTimeMillis()

  var cc_counter = 0
  println("start iterating")
  var i = 0
  var n = 0
  while (it.hasNext) {
    val line = it.next()
    i = i + 1
    if ((i % 100000) == 0) {
      n = n + 1
      println(line(match_type_index))
      println(n)
    }
    if (line(match_type_index).trim.equals( Alignment.TYPE_COMMENT_COMMENT)) {
      println(line)
    }
  }
}

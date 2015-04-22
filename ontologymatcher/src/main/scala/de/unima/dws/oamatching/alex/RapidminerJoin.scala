package de.unima.dws.oamatching.alex

import java.io.File

import com.github.tototoshi.csv.{CSVWriter, CSVReader}

import scala.collection.mutable

/**
 * Created by mueller on 22/04/15.
 */
object RapidminerJoin extends App {




  //id -> left, relation,right, owl_type, match_type
  val normal_vector_result_file = new File("original_vector.csv")

  val reader_normal = CSVReader.open(normal_vector_result_file)

  val norma_it = reader_normal.iterator

  val head_list = norma_it.next()

  val normal_index_to_name = head_list.zipWithIndex.map(tuple => tuple._2 -> tuple._1).toMap
  val normal_name_to_index = head_list.zipWithIndex.map(tuple => tuple._1 -> tuple._2).toMap
  var line:Seq[String] = null;
  val normMap= mutable.Map[Int,Seq[Array[Byte]]] ()
  var i = 0;
  while(norma_it.hasNext) {
    line =norma_it.next()
    i = i+1
    normMap( line(normal_name_to_index.get("id").get).toDouble.toInt ) =  Seq( line(normal_name_to_index.get("left").get).toCharArray.map(_.toByte),line(normal_name_to_index.get("relation").get).toCharArray.map(_.toByte),line(normal_name_to_index.get("right").get).toCharArray.map(_.toByte),line(normal_name_to_index.get("owl_type").get).toCharArray.map(_.toByte),line(normal_name_to_index.get("match_type").get).toCharArray.map(_.toByte) )
  }
  println(i)
  reader_normal.close()


  val metaDataFields = List("left", "relation", "right", "owl_type", "match_type", "outlier")
  val vector_result_file = new File("outlier_vector.csv")
  val reader_vector = CSVReader.open(vector_result_file)
  val outlier_vector_it = reader_vector.iterator
  val head_list_vector = outlier_vector_it.next()

  val normal_name_to_index_vector  = head_list_vector.zipWithIndex.map(tuple => tuple._1 -> tuple._2).toMap
  var line_vector:Seq[String] = null;
  var id = 0
  val writer = CSVWriter.open("joined.csv")

  //print Headline
  val header: List[String] = List[String]("left", "relation", "right", "owl_type", "match_type", "outlier", "equalSimilarityStemmed", "word2Vec", "prefix", "jaccard", "suffix", "smoaDistance", "jiangConrath", "simple_tfidf", "fuzzyjaccard", "levenshteinDistance", "hammingDistance", "jaroMeasure")
  writer.writeRow(header)


  while(outlier_vector_it.hasNext){
      line_vector = outlier_vector_it.next()

      id =  line_vector(normal_name_to_index_vector.get("id").get).toDouble.toInt

      if(normMap.get(id).isDefined){
        val meta: Seq[String] = normMap.get(id).get.map(new String(_))

        val row = meta ++ Seq(line_vector(normal_name_to_index_vector.get("outlier").get),
          line_vector(normal_name_to_index_vector.get("equalSimilarityStemmed").get),
          line_vector(normal_name_to_index_vector.get("word2Vec").get),
          line_vector(normal_name_to_index_vector.get("prefix").get),
          line_vector(normal_name_to_index_vector.get("jaccard").get),
          line_vector(normal_name_to_index_vector.get("suffix").get),
          line_vector(normal_name_to_index_vector.get("smoaDistance").get),
          line_vector(normal_name_to_index_vector.get("jiangConrath").get),
          line_vector(normal_name_to_index_vector.get("simple_tfidf").get),
          line_vector(normal_name_to_index_vector.get("fuzzyjaccard").get),
          line_vector(normal_name_to_index_vector.get("levenshteinDistance").get),
          line_vector(normal_name_to_index_vector.get("levenshteinDistance").get),
          line_vector(normal_name_to_index_vector.get("jaroMeasure").get)
          )

        writer.writeRow(row)

      }else {
        println("fail")
      }


  }
  reader_vector.close()

  writer.flush()
  writer.close()

}

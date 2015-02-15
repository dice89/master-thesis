package de.unima.dws.oamatching.thesis

import java.io.{PrintWriter, File}
import java.nio.file.Path

import com.github.tototoshi.csv.CSVWriter
import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.core.{Cell, AlignmentParser}
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.Runner
import de.unima.dws.oamatching.pipeline.evaluation.{EvaluationMatchingRunner, RdfFileFilter}

import scala.collection.parallel.immutable.ParVector
import scala.io.Source

/**
 * Created by mueller on 06/02/15.
 */
object MiscExperiments extends App {
  //MatcherRegistry.initLargeScale()

  //createVectorWithLabels()

  //printDataSetSizes()

  //bestMatcherForDataset()


  computePairWiseSimilarityForAllMatchings()
  def bestMatcherForDataset(): Unit = {
    val problems = EvaluationMatchingRunner.parseConference("ontos/2014/conference");
    val results = MatcherRegistry.matcher_by_name.keys.par.map(name => {
      val startime = System.currentTimeMillis()
      println(s"start $name matcher")
      val res = Runner.runSingleBaseMatcherForMultipleProblems(name, problems.toVector, 0.8, "conference")
      val endtime = System.currentTimeMillis() - startime

      println(s"finshed $name matcher in $endtime")

      (name, res, endtime)

    })
    val csv_file = new File("thesisexperiments/conference_results_base.csv")

    val writer = CSVWriter.open(csv_file)
    val header: List[String] = List[String]("matcher", "execution_time", "macro_precision", "macro_recall", "macro_f1", "micro_precision", "micro_recall", "micro_f1")

    //convert to java UTIL List
    writer.writeRow(header)

    results.seq.foreach(result => {

      val row = List(result._1, (result._3.toFloat / 1000), result._2.macro_eval_res.precision, result._2.macro_eval_res.recall, result._2.macro_eval_res.f1Measure, result._2.micro_eval_res.precision, result._2.micro_eval_res.recall, result._2.micro_eval_res.f1Measure)
      writer.writeRow(row)
    });

    writer.close()


  }

  def printDataSetSizes():Unit = {

    val pairs: List[(File, File)] = getListOfMatchingRefPairs

    pairs.foreach{case(ref_file,raw_file) => {
      val alignment = AlignmentParser.parseRDF(ref_file)
      val lines =  Source.fromFile(raw_file).getLines()
      val total_matchings = lines.size
      val alignment_size = alignment.correspondences.size
      val ratio = (alignment_size.toDouble/total_matchings.toDouble)*100
      val onto1 = alignment.onto1.replace("http://","")
      val onto2 = alignment.onto2.replace("http://","")
      println(s"$onto1-$onto2,$total_matchings,$alignment_size,$ratio")
    }}
  }


  def createVectorWithLabels(): Unit = {
    //read reference alignment folder

    val pairs: List[(File, File)] = getListOfMatchingRefPairs


    //write new csv with one more column :is matching?
   val ratios: List[Double] =  pairs.map{case (ref_file, raw_file) => {
      val name = raw_file.getName
      val writer = new PrintWriter(new File("thesisexperiments/matchings/"+name))
      val alignment = AlignmentParser.parseRDF(ref_file)

      val lines =  Source.fromFile(raw_file).getLines()
      val total_matchings = lines.size
      val alignment_size = alignment.correspondences.size
      lines.foreach(line => {
        val splitted = line.split(",")

        val left = splitted(0)

        val relation =splitted(1)
        val right = splitted(2)
        //type and measure here not imporatant

        val new_line = if(! left.equals("left")){
          //not first row
          val cell = new Cell(left,right,1.0,relation,"nn")
          if(alignment.correspondences.contains(cell)){
           line +",1"
          }else {
            line+",0"
          }
        }else {
         line +",isinRef"
        }
        writer.println(new_line)
      })
      writer.close()

      alignment_size.toDouble/total_matchings.toDouble
    }}

    val average_ratio = ratios.reduceLeft(_ + _)/ratios.length.toDouble

    println(average_ratio)
  }


  def getListOfMatchingRefPairs: List[(File, File)] = {
    val ref_folder: File = new File("ontos/2014/conference/reference-alignment")
    val list_of_references = ref_folder.listFiles(new RdfFileFilter()).toList


    //read matchings folder
    val file: File = new File("matchings")
    val list_of_raw_matchings = file.listFiles().toList

    //build pairs of matchings to reference alignment

    val pairs: List[(File, File)] = list_of_references.map(ref_align_file => {
      val ontos: List[String] = ref_align_file.getName().split("-").toList
      val name_onto1: String = ontos(0).replaceAll("-", "")
      val name_onto2: String = ontos(1).replaceAll("-", "").replaceAll(".rdf", "")


      list_of_raw_matchings.map(raw_file => {
        val id_part = raw_file.getName.split("_")(0)
        val splitted_id_part = id_part.split("-")
        if (splitted_id_part.length < 2) {
          Option.empty
        } else {
          if ((splitted_id_part(0).trim.equalsIgnoreCase(name_onto1) && splitted_id_part(1).trim.equalsIgnoreCase(name_onto2)) ||
            (splitted_id_part(1).trim.equalsIgnoreCase(name_onto1) && splitted_id_part(0).trim.equalsIgnoreCase(name_onto2))) {
            Option((ref_align_file, raw_file))
          } else {
            Option.empty
          }
        }
      }).filter(pair => pair.isDefined).head.get
    })
    pairs
  }


  def computePairWiseSimilarityForAllMatchings():Unit = {
    val file: File = new File("matchings")
    val list_of_raw_matchings = file.listFiles().toList

    list_of_raw_matchings.foreach(matchings =>{
      println(matchings.getName)
      println(computePairWiseSimilarity(matchings))
    })

  }


  def computePairWiseSimilarity(matching:File):(Int,Int, Double,Double,Double) ={

    val lines =  Source.fromFile(matching).getLines();
     val file_as_vector: ParVector[(Vector[Double], Int)] =  lines.map(line => {
         val vector = line.split(",")
         val real_vector: Vector[Double] = vector.map(elem => {

           try {
             Option(elem.toDouble)
           }
           catch {
             case other :Throwable => Option.empty
           }
         }).filter(_.isDefined).map(_.get).toVector
        real_vector
      }).toVector.filter(vector => vector.size > 1).zipWithIndex.par;

   /*val distances =  for((vector_a,index_a)<-file_as_vector;
        (vector_b,index_b)<-file_as_vector)yield {
      val euclidean_distance = computeEuclideanDistance(vector_a,vector_b)
      euclidean_distance
    }

    val mean = distances.reduceLeft(_+_)/distances.length

    //println(mean)

    val variance = distances.map(distance => Math.pow(distance -mean,2.0)).reduceLeft(_ + _)/distances.length

    //println(variance)

    val stdev = Math.sqrt(variance)
*/
    //println(stdev)

    (file_as_vector.size,file_as_vector.head._1.size,0.0,0.0,0.0)
    //(file_as_vector.size,file_as_vector.head._1.size,mean,variance,stdev)

  }

 def computeEuclideanDistance(a:Vector[Double], b:Vector[Double]):Double = {
   val squared_diff: Vector[Double] = a.zip(b).map(tuple => Math.pow(tuple._1 - tuple._2,2.0))

   Math.sqrt(squared_diff.reduceLeft(_ + _))
 }
}
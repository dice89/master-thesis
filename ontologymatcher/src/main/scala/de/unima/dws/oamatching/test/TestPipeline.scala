package de.unima.dws.oamatching.test

import java.io.File

import de.unima.dws.oamatching.core.{AlignmentParser, Alignment, OntologyLoader}
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.matcher.elementlevel.SimpleStringFunctionMatcher
import de.unima.dws.oamatching.measures.StringMeasures
import org.apache.spark.{SparkContext, SparkConf}

/**
 * Created by mueller on 21/01/15.
 */
object TestPipeline extends App{

    val test:Map[String,Int] = Map(("a",1),("b",2),("c",3))

  val test_list1 = test.keys.toList.zipWithIndex
  println(test_list1)

  val test_list2 = test.keys.toList.zipWithIndex
  println(test_list2)

  val test_list3 = test.values.toList
  println(test_list3)

 /* val conf = new SparkConf()
   .setAppName("Simple Application")
   //this needs to be parameterized.
   .setMaster("local[1]")
   .set("spark.executor.memory", "1g")

 val sc = new SparkContext(conf)*/




/*
  val file_aling1: File = new File("ontos/2014/conference/reference-alignment/cmt-iasted.rdf");
  val reference = AlignmentParser.parseRDF(file_aling1)

  val onto1 =  OntologyLoader.load("ontos/2014/conference/cmt.owl");

  val onto2 = OntologyLoader.load("ontos/2014/conference/iasted.owl");
 MatcherRegistry.init
 println("start matching")

  for(matcher <- MatcherRegistry.matcher_by_name) {
   println("start first matcher")
   val alignment:Alignment= matcher._2.align(onto1,onto2,0.6)
   //println(matcher._1 + " --- " + alignment.evaluate(reference).toString);*/





}

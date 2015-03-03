package de.unima.dws.oamatching.thesis.scalabilityTests

import de.unima.dws.oamatching.core.{MatchingCell, Alignment, Cell}
import de.unima.dws.oamatching.measures.StringMeasures

/**
 * Created by mueller on 02/03/15.
 */


case class Tester(left:String,right:String, relation:String, measure:Double, owl_type:String)
object Test extends App{


  val size:Int = 3000
  val test_data1 = generatedData(size)
  val test_data2 = generatedData(size)


  val test1_indexed_seq = test_data1.toVector
  val test2_indexed_seq = test_data2.toVector

  /*
  val startime2 = System.currentTimeMillis()
  val alignment2:Alignment =  new Alignment(null,null)

  val iter1 = test_data1.toIterator
  while(iter1.hasNext){
    val entity1 = iter1.next()
    val iter2 = test_data2.toIterator

    while (iter2.hasNext){
      val entity2 = iter2.next()

      alignment2.test2.add(entity1,entity2,0.0,"=","")

    }
  }

  println("done2 in "+(System.currentTimeMillis()-startime2))
  println(alignment2.test2.size)



  val startime1 = System.currentTimeMillis()
  println("start1")

  val alignment1:Alignment =  new Alignment(null,null)
  for(entity1 <-test_data1.view; entity2<-test_data2.view){

    alignment1.test2.add(entity1,entity2,0.0,"=","")
  }

  println("done1 in "+(System.currentTimeMillis()-startime1))
  println(alignment1.test2.size)


  val test1_seq = test_data1.toSeq.par
  val test2_seq = test_data2.toSeq.par
  val startime3 = System.currentTimeMillis()
  val alignment3 = new Alignment(null,null)



  test1_seq.foreach(entity1 => {
    test2_seq.foreach(entity2 => {
      alignment3.test2.add(entity1,entity2,0.0,"=","")
    })
  })





  println("done3 in "+(System.currentTimeMillis()-startime3))
  println(alignment1.test2.size)

  */

  val alignment0:Alignment =  new Alignment(null,null)
  val startime0 = System.currentTimeMillis()
  var i = 0
  var n = 0
  var entity1=""
  var entity2=""
  while (i < test1_indexed_seq.size){
    n = 0
    entity1 = test1_indexed_seq(i)
    while(n < test2_indexed_seq.size){
     entity2 = test2_indexed_seq(n)
      val measure = StringMeasures.computeJaccard(entity1,entity2)
      if(measure > 0.1){
        //alignment0.test3.add(entity1,entity2,measure,"=","")
        alignment0.correspondences.add(MatchingCell(entity1,entity2,measure,"=",""))
      }

      n = n+1
    }
    i = i+1
  }

  //println("done0 in "+(System.currentTimeMillis()-startime0) + "total size "+ alignment0.test2.size)
  //TODO try it without iterator and random access an vector




  def generatedData(size:Int): Array[String] = {
    val test_set = new Array[String](size)

    test_set.zipWithIndex.map(value => "test_value_"+value._2)
  }




}

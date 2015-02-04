package de.unima.dws.oamatching.measures

import com.wcohen.ss.Jaccard
import com.wcohen.ss.MongeElkan
import com.wcohen.ss.tokens.SimpleTokenizer

/**
 * Created by mueller on 21/01/15.
 */
object StringMeasures {

      def measureEquality(a:String,b:String) :Double = {
        if(a.equals(b)) {
          1.0
        }else {
          0.0
        }
      }

  /**
   * Computes if the string b starts with parts of string a
   * @param a
   * @param b
   * @return
   */
  def computePrefixBiDirectional(a: String, b: String): Double = {
    val res_a_b = computeAnyfixUniDirectional(b.toLowerCase.startsWith)(a, b)
    val res_b_a = computeAnyfixUniDirectional(a.toLowerCase.startsWith)(b, a)
    Math.max(res_a_b, res_b_a)
  }

  /**
   * Computes if the string b ends with parts of string a
   * @param a
   * @param b
   * @return
   */
  def computeSuffixBiDirectional(a: String, b: String): Double = {
    val res_a_b = computeAnyfixUniDirectional(b.toLowerCase.endsWith)(a, b)
    val res_b_a = computeAnyfixUniDirectional(a.toLowerCase.endsWith)(b, a)
    Math.max(res_a_b, res_b_a)
  }

  /**
   * General function to compare two string with their prefix or suffix
   * @param anyfixfunction endsWith oder startsWith string function
   * @param substring
   * @param superstring
   * @return
   */
  private def computeAnyfixUniDirectional(anyfixfunction: (String) => Boolean)(substring: String, superstring: String): Double = {
    //first check if subsequence of min length 2 are a is Anyfix of b
    val res_values = for { seq_size <- 1 until substring.length() + 1; subseq <- substring.toLowerCase.sliding(seq_size, 1).toSeq } yield {
      if (anyfixfunction(subseq)) {
        Some(seq_size.asInstanceOf[Double] / substring.length().asInstanceOf[Double])
      } else {
        None
      }
    }

    val a_b_score = res_values.reduceLeft((A, B) => {
      if (A.getOrElse(0.0) > B.getOrElse(0.0)) {
        A
      } else {
        B
      }
    })

    a_b_score.getOrElse(0.0)
  }

  @Deprecated
  private def computePrefixUniDirectional(substring: String, superstring: String): Double = {
    //first check if subsequence of min length 2 are a is Anyfix of b
    val res_values = for { seq_size <- 1 until substring.length() + 1; subseq <- substring.sliding(seq_size, 1).toSeq } yield {
      if (superstring.startsWith(subseq)) {
        Some(seq_size.asInstanceOf[Double] / substring.length().asInstanceOf[Double])
      } else {
        None
      }
    }

    val a_b_score = res_values.reduceLeft((A, B) => {
      if (A.getOrElse(0.0) > B.getOrElse(0.0)) {
        A
      } else {
        B
      }
    })

    a_b_score.getOrElse(0.0)
  }
  /**
   * Simple wrapper function for second string jaccard distance, when calling it, make sure you use a stemmer before
   * @param a
   * @param b
   * @return
   */
  def computeJaccard(a: String, b: String): Double = {
    val jaccard: Jaccard = new Jaccard(new SimpleTokenizer(true, false))

    val res = jaccard.score(jaccard.prepare(a.toLowerCase), jaccard.prepare(b.toLowerCase))

    res
  }

  /**
   * Simple wrapper function for second string mongeElkan similarity
   * @param a
   * @param b
   * @return
   */
  def computeMongeElkan(a: String, b: String): Double = {

    val mongeElkan: MongeElkan = new MongeElkan()
    //scale result from 0-1
    mongeElkan.setScaling(true)
    //perform score
    mongeElkan.score(a.toLowerCase, b.toLowerCase)
  }

  /**
   * Compute Lin Measure based on WS4Jƒƒ
   * @param a
   * @param b
   * @return
   */
  def computeLin(a: String, b: String): Double = {

    var res = WordNetMeasureHelper.lin.calcRelatednessOfWords(a, b)
    //println(res)

    //if they are the same he score is Double.MaxValue => normalize to 1
    if (res > 1.0) {
      res = 1.0
    }

    if (res == -0.0) {
      res = 0.0
    }

    res
  }

  /**
   *  Computes Jiang Conrath Measure base on WS4J
   * @param a
   * @param b
   * @return
   */
  def computeJiangConrath(a: String, b: String): Double = {

    var res = WordNetMeasureHelper.jianConrath.calcRelatednessOfWords(a, b)
    //if they are the same he score is Double.MaxValue => normalize to 1

    if (res > 1.0) {
      res = 1.0
    }
    res
  }

  /**
   * Computes Wu Palmer Measure from WS4J
   * @param a
   * @param b
   * @return
   */
  def computeWuPalmer(a: String, b: String): Double = {
    var res = WordNetMeasureHelper.wuPalmer.calcRelatednessOfWords(a, b)
    //if they are the same he score is Double.MaxValue => normalize to 1
    if (res > 1.0) {
      res = 1.0
    }
    res
  }

  def computePath(a: String, b: String): Double = {
    var res = WordNetMeasureHelper.path calcRelatednessOfWords (a, b)
    //if they are the same he score is Double.MaxValue => normalize to 1
    if (res > 1.0) {
      res = 1.0
    }
    res
  }

}
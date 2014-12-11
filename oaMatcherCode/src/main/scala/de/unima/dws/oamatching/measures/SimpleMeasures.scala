package de.unima.dws.oamatching.measures

import com.wcohen.ss.Jaccard
import com.wcohen.ss.tokens.SimpleTokenizer
import com.wcohen.ss.MongeElkan

object SimpleMeasures {

  /**
   * Simple wrapper function for second string jaccard distance, when calling it, make sure you use a stemmer before
   * @param a
   * @param b
   * @return
   */
  def computeJaccard(a: String, b: String): Double = {
    val jaccard: Jaccard = new Jaccard(new SimpleTokenizer(true, false))

    val res = jaccard.score(jaccard.prepare(a), jaccard.prepare(b))
    /*if(res >0.0){
      println(jaccard.explainScore(a, b))
    }*/
    res
  }

  /**
   * Simple wrapper function for second string mongeElkan similarity
   * @param a
   * @param b
   * @return
   */
  def computeMongeElkan(a: String, b: String): Double = {

    val mongeElkan: MongeElkan = new MongeElkan();
    //scale result from 0-1
    mongeElkan.setScaling(true)
    //perform score
    mongeElkan.score(a, b)
  }
}
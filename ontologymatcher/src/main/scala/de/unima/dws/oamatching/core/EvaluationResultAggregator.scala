package de.unima.dws.oamatching.core

/**
 * Created by mueller on 22/01/15.
 */

case class EvaluationResult(precision:Double, recall:Double, f1Measure:Double , truePositives:Int, falsePositives:Int,FalseNegatives:Int)

object EvaluationResultAggregator {

  /**
   * Return and EvaluationResult Object based on tp,fp,fn
   * @param truePositives
   * @param falsePositives
   * @param falseNegatives
   * @return
   */
  def createEvaluationResult(truePositives:Int, falsePositives:Int,falseNegatives:Int):EvaluationResult = {
    val precision:Double = (truePositives).toDouble /(truePositives+falsePositives).toDouble

    val recall:Double = (truePositives).toDouble /(truePositives+falseNegatives).toDouble

    val fMeasure:Double = 2*((precision*recall)/(precision+recall))

    EvaluationResult(precision,recall,fMeasure,truePositives,falsePositives,falseNegatives)
  }

}

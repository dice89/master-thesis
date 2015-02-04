package de.unima.dws.oamatching.core

/**
 * Created by mueller on 22/01/15.
 */

case class EvaluationResult(precision:Double, recall:Double, f1Measure:Double , truePositives:Int, falsePositives:Int,FalseNegatives:Int);

case class AggregatedEvaluationResult(micro_eval_res :EvaluationResult, macro_eval_res:EvaluationResult)

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

  /**
   * Aggregate Results in Micro and Macro average way
   * @param evaluation_results
   * @return
   */
  def aggregateEvaluationResults(evaluation_results:List[EvaluationResult]):AggregatedEvaluationResult = {

    val tupled_eval_res = {
      evaluation_results.map(eval_res => EvaluationResult.unapply(eval_res).get)
    }
    val no_of_result = evaluation_results.size
    val sum =  EvaluationResult.tupled(tupled_eval_res.reduceLeft[(Double,Double,Double,Int,Int,Int)]{case(previous, tuples) => EvaluationResult.unapply(EvaluationResult(previous._1+tuples._1,previous._2+tuples._2, previous._3+tuples._3 ,previous._4+tuples._4, previous._5+tuples._5, previous._6+tuples._6 )).get})

    val macro_average = EvaluationResult(sum.precision/no_of_result.toDouble,
      sum.recall/no_of_result.toDouble,
      sum.f1Measure/no_of_result.toDouble,
      sum.truePositives,
      sum.falsePositives,
      sum.FalseNegatives)

    //micro avg calc
    val precision:Double = (sum.truePositives).toDouble /(sum.truePositives+sum.falsePositives).toDouble

    val recall:Double = (sum.truePositives).toDouble /(sum.truePositives+sum.FalseNegatives).toDouble

    val fMeasure:Double = 2*((precision*recall)/(precision+recall))

    val micro_avg =  EvaluationResult(precision,recall,fMeasure,sum.truePositives,sum.falsePositives,sum.FalseNegatives)

    AggregatedEvaluationResult(micro_avg,macro_average)

  }

  /**
   * Aggregate but with optionated data
   * @param evaluation_results
   * @return
   */
  def aggregateEvaluationResultsOption(evaluation_results:List[Option[EvaluationResult]]):AggregatedEvaluationResult = {
    //get results tupled
    val filtered_eval_res = evaluation_results.filter(eval_res => eval_res.isDefined).map(elm => elm.get);

    aggregateEvaluationResults(filtered_eval_res)

  }

}

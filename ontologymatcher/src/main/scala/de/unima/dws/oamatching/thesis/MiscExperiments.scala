package de.unima.dws.oamatching.thesis

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.Runner
import de.unima.dws.oamatching.pipeline.evaluation.EvaluationMatchingRunner

/**
 * Created by mueller on 06/02/15.
 */
object MiscExperiments extends  App{
  MatcherRegistry.initLargeScale()


  bestMatcherForDataset()

  def bestMatcherForDataset():Unit = {
    val problems = EvaluationMatchingRunner.parseConference("ontos/2014/conference");
    val results =  MatcherRegistry.matcher_by_name.keys.par.map(name  =>  {
      val startime = System.currentTimeMillis()
      println(s"start $name matcher")
      val res = Runner.runSingleBaseMatcherForMultipleProblems(name,problems.toVector,0.8,"conference" )
      val endtime = System.currentTimeMillis() -startime

      println(s"finshed $name matcher in $endtime")

      (name,res,endtime )

    })
    val csv_file = new File("thesisexperiments/conference_results_base.csv")

    val writer = CSVWriter.open(csv_file)
    val header: List[String] = List[String]("matcher", "execution_time", "macro_precision", "macro_recall", "macro_f1", "micro_precision", "micro_recall", "micro_f1")

    //convert to java UTIL List
    writer.writeRow(header)

    results.seq.foreach(result => {

      val row = List(result._1, (result._3.toFloat/1000), result._2.macro_eval_res.precision,result._2.macro_eval_res.recall, result._2.macro_eval_res.f1Measure,  result._2.micro_eval_res.precision,result._2.micro_eval_res.recall, result._2.micro_eval_res.f1Measure)
      writer.writeRow(row)
    });

    writer.close()


  }

}

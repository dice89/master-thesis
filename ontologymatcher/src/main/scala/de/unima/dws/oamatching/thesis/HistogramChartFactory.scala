package de.unima.dws.oamatching.thesis

import java.awt.Graphics2D
import java.awt.geom.Rectangle2D
import java.io.FileOutputStream

import com.itextpdf.awt.PdfGraphics2D
import com.itextpdf.text
import com.itextpdf.text._
import com.itextpdf.text.pdf._
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.pipeline.evaluation.EvaluationMatchingRunner
import org.jfree.chart.axis.LogAxis
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.data.statistics.HistogramDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import scala.collection.immutable.Map
;

/**
 * Singleton to create some vizualizations of the data sets
 * Created by mueller on 18/02/15.
 */
object HistogramChartFactory{

  def createExecutionSummaryReport(folder:String,name:String,best_result: (String, (Map[String, Map[String, Double]], ProcessEvalExecutionResultsNonSeparated))):Unit = {
    val document: Document = new Document();
    val writer: PdfWriter = PdfWriter.getInstance(document, new FileOutputStream(folder+"/"+name+".pdf"));

    document.open()
    val best_result_res = best_result._2

    val anchor = new Anchor("Results of Analysis for: " + name);
    anchor.setName("results");

    // Second parameter is the number of the chapter
    val catPart = new Chapter(new Paragraph(anchor), 1);

    val section1 = catPart.addSection("Overview")

    val table: PdfPTable = new PdfPTable(2);

    val c1: PdfPCell = new PdfPCell(new Phrase("Field"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c2: PdfPCell = new PdfPCell(new Phrase("Value"));
    c2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c2);

    table.addCell(new Phrase("Algorithm "))
    table.addCell(new Phrase(name))

    table.addCell(new Phrase("Pre Pro Technique:"))
    table.addCell(new Phrase(best_result_res._1.toString))

    table.addCell(new Phrase("Process: "))
    table.addCell(new Phrase(best_result_res._2.best_result._1))

    table.addCell(new Phrase("Separated? "))
    table.addCell(new Phrase(best_result_res._2.separated.toString))


    if(best_result_res._2.separated){

      table.addCell(new Phrase("Norm Technique: "))
      table.addCell(new Phrase( best_result_res._2.best_result._2.best_separated_result._1))

      table.addCell(new Phrase("class threshold: "))
      table.addCell(new Phrase( best_result_res._2.best_result._2.best_separated_result._2.c_threshold.toString))

      table.addCell(new Phrase("dp threshold: "))
      table.addCell(new Phrase( best_result_res._2.best_result._2.best_separated_result._2.dp_threshold.toString))

      table.addCell(new Phrase("op threshold: "))
      table.addCell(new Phrase( best_result_res._2.best_result._2.best_separated_result._2.op_threshold.toString))

    }else {

      table.addCell(new Phrase("Norm Technique: "))
      table.addCell(new Phrase( best_result_res._2.best_result._2.best_result._1))

      table.addCell(new Phrase("Threshold: "))
      table.addCell(new Phrase(best_result_res._2.best_result._2.best_result._2._1.toString))

    }


    table.addCell(new Phrase("Result (Macro)"))
    table.addCell(new Phrase(best_result_res._2.overall_agg_best.macro_eval_res.toString))

    table.addCell(new Phrase("Result (Micro)"))
    table.addCell(new Phrase(best_result_res._2.overall_agg_best.micro_eval_res.toString))

    section1.add(table)
    val para_config = best_result_res._1


    val section2 = catPart.addSection("Parameter Configuration")

    val table2: PdfPTable = new PdfPTable(3);

    val c1_2: PdfPCell = new PdfPCell(new Phrase("Category"));
    c1_2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table2.addCell(c1_2);

    val c2_2: PdfPCell = new PdfPCell(new Phrase("Parameter"));
    c2_2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table2.addCell(c2_2);


    val c3_2: PdfPCell = new PdfPCell(new Phrase("Value"));
    c3_2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table2.addCell(c3_2);

    para_config.foreach{case(category,param_map)=>{
      param_map.foreach{case(parameter,value)=>{
        table2.addCell(category)
        table2.addCell(parameter)
        table2.addCell(value.toString)
      }}
    }}
    section2.add(table2)

    document.add(catPart)

    document.close()
  }

  def createReportForExecutionRun(folder:String,name:String, results:ProcessEvalExecutionResultsNonSeparated,para_config:Map[String, Map[String, Double]]): Unit ={
    val document: Document = new Document();
    val writer: PdfWriter = PdfWriter.getInstance(document, new FileOutputStream(folder+"/"+name+".pdf"));

    // step 3
    document.open();

    val cb: PdfContentByte = writer.getDirectContent();
    val chart_width = PageSize.A4.getWidth().toFloat
    val chart_height = (PageSize.A4.getHeight() / 2).toFloat;


    //title page
    document.add(startingTable(name, results,para_config))
    document.newPage()
    var i = 2;
    results.results.foreach{case(name,result)=>{
        val anchor = new Anchor("Outlier Analysis process technique " + name);
        val chapter = new Chapter(new Paragraph(anchor), i);
        if(result.separated){

          printBestResultSeparated(name,result.best_separated_result._1,result.best_separated_result._2,chapter)
          chapter.newPage()
          printGlobalThresholdsPageSeparated(name, result.best_separated_results,chapter)
          chapter.newPage()

        }else {
          //print best global result
          printBestResult(name,result.best_result,chapter)
          chapter.newPage()
          //print global thresholds
          printGlobalThresholdsPage(name,result.local_global_threshold.best_global_results, chapter)
          chapter.newPage()

          //print local thresholds
          printLocalThresholdsPage(name,result.local_global_threshold.agg_local_optimum,result.local_global_threshold.local_optima_per_ds, chapter)
          chapter.newPage()

          //print top results pages
          printPDFTopNEvaluatedPage(result.top_n_results, chapter)
          chapter.newPage()
          val title = new Paragraph("Precision Recall Curves for " +name)
          val section = chapter.addSection(title)
          document.add(chapter)
          document.newPage()
          printPrecisionRecallChart(result.local_global_threshold.global_results_per_threshold, cb,chart_width,chart_height,document)

        }


        i = i+1
    }}

    document.close()


  }

  def printBestResultSeparated(name:String, norm_technique:String, result:SeparatedResult, chapter:Chapter):Unit ={
    val title = new Paragraph("Best Result for  " + name);

    val section = chapter.addSection(title)
    // Second parameter is the number of the chapter

    val table: PdfPTable = new PdfPTable(2);

    val c1: PdfPCell = new PdfPCell(new Phrase("Field"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c2: PdfPCell = new PdfPCell(new Phrase("Value"));
    c2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c2);

    table.addCell(new Phrase("Norm Technique"))
    table.addCell(new Phrase(norm_technique))

    table.addCell(new Phrase("class threshold"))
    table.addCell(new Phrase(result.c_threshold.toString))

    table.addCell(new Phrase("dp threshold"))
    table.addCell(new Phrase(result.dp_threshold.toString))

    table.addCell(new Phrase("op threshold"))
    table.addCell(new Phrase(result.op_threshold.toString))

    table.addCell(new Phrase("F1 Measure (Macro)"))
    table.addCell(new Phrase(result.result.macro_eval_res.f1Measure.toString))

    table.addCell(new Phrase("Precision (Macro)"))
    table.addCell(new Phrase(result.result.macro_eval_res.precision.toString))

    table.addCell(new Phrase("Recall (Macro)"))
    table.addCell(new Phrase(result.result.macro_eval_res.recall.toString))

    table.addCell(new Phrase("F1 Measure (Micro)"))
    table.addCell(new Phrase(result.result.micro_eval_res.f1Measure.toString))

    table.addCell(new Phrase("Precision (Micro)"))
    table.addCell(new Phrase(result.result.micro_eval_res.precision.toString))

    table.addCell(new Phrase("Recall (Micro)"))
    table.addCell(new Phrase(result.result.micro_eval_res.recall.toString))

  }


  def printBestResult(name: String, results: (String, (Double, AggregatedEvaluationResult)), chapter:Chapter):Unit = {
    val title = new Paragraph("Best Result for  " + name);

    val section = chapter.addSection(title)
    // Second parameter is the number of the chapter

    val best_result = results
    val table: PdfPTable = new PdfPTable(2);

    val c1: PdfPCell = new PdfPCell(new Phrase("Field"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c2: PdfPCell = new PdfPCell(new Phrase("Value"));
    c2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c2);

    table.addCell(new Phrase("Norm Technique"))
    table.addCell(new Phrase(best_result._1))

    table.addCell(new Phrase("threshold"))
    table.addCell(new Phrase(best_result._2._1.toString))

    table.addCell(new Phrase("F1 Measure (Macro)"))
    table.addCell(new Phrase(best_result._2._2.macro_eval_res.f1Measure.toString))

    table.addCell(new Phrase("Precision (Macro)"))
    table.addCell(new Phrase(best_result._2._2.macro_eval_res.precision.toString))

    table.addCell(new Phrase("Recall (Macro)"))
    table.addCell(new Phrase(best_result._2._2.macro_eval_res.recall.toString))

    table.addCell(new Phrase("F1 Measure (Micro)"))
    table.addCell(new Phrase(best_result._2._2.micro_eval_res.f1Measure.toString))

    table.addCell(new Phrase("Precision (Micro)"))
    table.addCell(new Phrase(best_result._2._2.micro_eval_res.precision.toString))

    table.addCell(new Phrase("Recall (Micro)"))
    table.addCell(new Phrase(best_result._2._2.micro_eval_res.recall.toString))


    section.add(table)

  }


  def startingTable(name: String, results: ProcessEvalExecutionResultsNonSeparated, para_config:Map[String, Map[String, Double]]):Element = {
    val anchor = new Anchor("Results of Analysis for: " + name);
    anchor.setName("results");

    // Second parameter is the number of the chapter
    val catPart = new Chapter(new Paragraph(anchor), 1);

    val section1 = catPart.addSection("Overview")
    val paragraph = new Paragraph("Best result based on threshold optimization is: " + results.best_result._1)
    section1.add(paragraph)

    val best_result = results.best_result._2.best_result
    val table: PdfPTable = new PdfPTable(2);

    val c1: PdfPCell = new PdfPCell(new Phrase("Field"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c2: PdfPCell = new PdfPCell(new Phrase("Value"));
    c2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c2);

    table.addCell(new Phrase("Process"))
    table.addCell(new Phrase(results.best_result._1))


    table.addCell(new Phrase("C/DP/OP separated?"))
    table.addCell(new Phrase(results.separated.toString))

    if(!results.separated){
      table.addCell(new Phrase("Norm Technique"))
      table.addCell(new Phrase(best_result._1))

      table.addCell(new Phrase("threshold"))
      table.addCell(new Phrase(best_result._2._1.toString))
    }else {
      table.addCell(new Phrase("Norm Technique"))
      table.addCell(new Phrase(results.best_result._2.best_separated_result._1))

      table.addCell(new Phrase("Thresholds"))
      table.addCell(new Phrase(results.best_result._2.best_separated_result._2.toString))
    }

    table.addCell(new Phrase("F1 Measure (Macro)"))
    table.addCell(new Phrase(results.overall_agg_best.macro_eval_res.f1Measure.toString))

    table.addCell(new Phrase("Precision (Macro)"))
    table.addCell(new Phrase(results.overall_agg_best.macro_eval_res.precision.toString))

    table.addCell(new Phrase("Recall (Macro)"))
    table.addCell(new Phrase(results.overall_agg_best.macro_eval_res.recall.toString))

    table.addCell(new Phrase("F1 Measure (Micro)"))
    table.addCell(new Phrase(results.overall_agg_best.micro_eval_res.f1Measure.toString))

    table.addCell(new Phrase("Precision (Micro)"))
    table.addCell(new Phrase(results.overall_agg_best.micro_eval_res.precision.toString))

    table.addCell(new Phrase("Recall (Micro)"))
    table.addCell(new Phrase(results.overall_agg_best.micro_eval_res.recall.toString))


    section1.add(table)


    val section2 = catPart.addSection("Parameter Configuration")

    val table2: PdfPTable = new PdfPTable(3);

    val c1_2: PdfPCell = new PdfPCell(new Phrase("Category"));
    c1_2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table2.addCell(c1_2);

    val c2_2: PdfPCell = new PdfPCell(new Phrase("Parameter"));
    c2_2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table2.addCell(c2_2);


    val c3_2: PdfPCell = new PdfPCell(new Phrase("Value"));
    c3_2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table2.addCell(c3_2);

    para_config.foreach{case(category,param_map)=>{
      param_map.foreach{case(parameter,value)=>{
        table2.addCell(category)
        table2.addCell(parameter)
        table2.addCell(value.toString)
      }}
    }}
    section2.add(table2)


    catPart
  }

  def createHistogramForOutlierScores(results: Map[MatchRelation, Double], filename: String): (JFreeChart, JFreeChart) = {

    val histogramdataset: HistogramDataset = new HistogramDataset();
    val values = results.values.toArray
    histogramdataset.addSeries("Outlier Values", values, 100)
    val log_chart: JFreeChart = ChartFactory.createHistogram(filename + " log scale", null, null, histogramdataset, PlotOrientation.VERTICAL, true, true, false);
    val rangeAxis: LogAxis = new LogAxis("Log(frequency)");
    log_chart.getXYPlot.setRangeAxis(rangeAxis)
    //ChartUtilities.saveChartAsPNG(new File("thesisexperiments/histograms/"+filename+"log_scale.PNG"), log_chart, 600, 400);

    val normal_chart: JFreeChart = ChartFactory.createHistogram(filename, null, null, histogramdataset, PlotOrientation.VERTICAL, true, true, false);
    //ChartUtilities.saveChartAsPNG(new File("thesisexperiments/histograms/"+filename+"normal_scale.PNG"), normal_chart, 600, 400);

    (log_chart, normal_chart)
  }

  def storeMultipleChartsIntoPDF(charts: Seq[(Seq[(JFreeChart, JFreeChart)], OutlierEvalStatisticsObject, Seq[Seq[(MatchRelation, Double)]], Alignment, Alignment, Map[String, (Map[MatchRelation, Double], Alignment)])], output_file: String, optimal_thresholds: (Map[String, (Double, AggregatedEvaluationResult)], Map[String, AggregatedEvaluationResult], Map[String, Seq[(Double, EvaluationResult)]]), precision_recall_data: Map[String, Seq[(Double, AggregatedEvaluationResult)]]): Unit = {
    val document: Document = new Document();
    val writer: PdfWriter = PdfWriter.getInstance(document, new FileOutputStream(output_file));

    // step 3
    document.open();
    // step 4
    val cb: PdfContentByte = writer.getDirectContent();
    val width = PageSize.A4.getWidth().toFloat
    val height = (PageSize.A4.getHeight() / 2).toFloat;

    //compute aggregated result

   // document.add(printGlobalThresholdsPage(optimal_thresholds._1))
    document.newPage()

    //document.add(printLocalThresholdsPage(optimal_thresholds._2, optimal_thresholds._3))
    document.newPage()

  //  printPrecisionRecallChart(precision_recall_data, cb, width - 20, height - 30, document)

    for (chart_tuple <- charts) {
      document.add(printPDFStatisticsPage(chart_tuple._2))
      document.newPage()

      for (top_n <- chart_tuple._3) {
        document.add(printPDFTopNPage(top_n, chart_tuple._2.ds_name, chart_tuple._4))
        document.newPage()
      }

      //print charts
      for (chart <- chart_tuple._1) {
        printPDFChartPage(cb, width, height - 30, chart)
        document.newPage()
      }

    }

    document.close
  }

  def printPrecisionRecallChart(precision_recall_data: Map[String, Seq[(Double, AggregatedEvaluationResult)]], cb: PdfContentByte, width: Float, height: Float, document:Document): Unit = {

    val series_per_technique = precision_recall_data.map { case (technique, results) => {
      val series_micro: XYSeries = new XYSeries(technique + " Micro Average");
      val series_macro: XYSeries = new XYSeries(technique + " Macro Average");
      results.foreach { case (threshold, eval_res) => {
        series_micro.add(eval_res.micro_eval_res.precision, eval_res.micro_eval_res.recall)
        series_macro.add(eval_res.macro_eval_res.precision, eval_res.macro_eval_res.recall)
      }
      }
      technique ->(series_micro, series_macro)
    }
    }


    series_per_technique.foreach { case (technique, series) => {
      val dataset: XYSeriesCollection = new XYSeriesCollection();
      dataset.addSeries(series._1);
      dataset.addSeries(series._2);

      val xylineChart: JFreeChart = ChartFactory.createXYLineChart(
        s"Precision Recall curve of $technique",
        "Precision",
        "Recall",
        dataset,
        PlotOrientation.VERTICAL,
        true, true, false);


      val chart1: PdfTemplate = cb.createTemplate(width, height);
      val g2d1: Graphics2D = new PdfGraphics2D(chart1, width, height);
      val r2d1: Rectangle2D = new Rectangle2D.Double(0, 0, width, height);

      xylineChart.draw(g2d1, r2d1)

      g2d1.dispose();
      cb.addTemplate(chart1, 0, height);
      document.newPage()
    }
    }
  }


  def printLocalThresholdsPage(process_name:String,agg_local_result: Map[String, AggregatedEvaluationResult], best_thresholds: Map[String, Seq[(Double, EvaluationResult)]], chapter:Chapter) = {

    val title = new Paragraph("Local threshold optimization results" + process_name)
    val section = chapter.addSection(title)

    val table: PdfPTable = new PdfPTable(3);

    val c1: PdfPCell = new PdfPCell(new Phrase("Norm Technique"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c3: PdfPCell = new PdfPCell(new Phrase("F1 Macro"));
    c3.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c3);

    val c4: PdfPCell = new PdfPCell(new Phrase("F1 Micro"))
    c4.setHorizontalAlignment(Element.ALIGN_CENTER)
    table.addCell(c4)

    agg_local_result.foreach(tuple => {
      table.addCell(tuple._1)
      table.addCell(tuple._2.macro_eval_res.f1Measure.toString)
      table.addCell(tuple._2.micro_eval_res.f1Measure.toString)
    })

    section.add(table)

    section.newPage()

    best_thresholds.foreach { case (norm_technique, thresholds) => {
      section.add(printBestLocalThresholdTable(norm_technique, thresholds))
      section.newPage()
    }
    }

  }


  def printBestLocalThresholdTable(norm_technique: String, thresholds: Seq[(Double, EvaluationResult)]): Element = {

    // Second parameter is the number of the chapter
    val catPart = new Paragraph(s"Local threshold optimization results for $norm_technique")

    val table: PdfPTable = new PdfPTable(2);

    val c3: PdfPCell = new PdfPCell(new Phrase("F1 Macro"));
    c3.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c3);

    val c4: PdfPCell = new PdfPCell(new Phrase("Threshold"));
    c4.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c4);

    thresholds.foreach { case (threshold, eval_res) => {
      table.addCell(eval_res.f1Measure.toString)
      table.addCell(threshold.toString)
    }
    }

    val thresholds_array = thresholds.map(_._1).toArray
    val stdev = CreateOutlierScoreStatistics.stdev_computer.evaluate(thresholds_array)
    val mean = CreateOutlierScoreStatistics.mean_computer.evaluate(thresholds_array)

    catPart.add(table)

    catPart.add(new Paragraph("STDEV Threshold = " + stdev))
    catPart.add(new Paragraph("Mean Threshold = " + mean))

    catPart
  }

  def printGlobalThresholdsPageSeparated(process_name:String,best_thresholds: Map[String,SeparatedResult], chapter: Chapter) = {
    val title = new Paragraph("Global Threshold optimization results" + process_name)
    val section = chapter.addSection(title)

    val table: PdfPTable = new PdfPTable(6);

    val c1: PdfPCell = new PdfPCell(new Phrase("Norm Technique"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c2: PdfPCell = new PdfPCell(new Phrase("c threshold"));
    c2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c2);

    val c5: PdfPCell = new PdfPCell(new Phrase("dp threshold"));
    c5.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c5);

    val c6: PdfPCell = new PdfPCell(new Phrase("op threshold"));
    c6.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c6);



    val c3: PdfPCell = new PdfPCell(new Phrase("F1 Macro"));
    c3.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c3);

    val c4: PdfPCell = new PdfPCell(new Phrase("F1 Micro"));
    c4.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c4);

    best_thresholds.foreach(tuple => {
      table.addCell(tuple._1)
      table.addCell(tuple._2.c_threshold.toString)
      table.addCell(tuple._2.dp_threshold.toString)
      table.addCell(tuple._2.op_threshold.toString)
      table.addCell(tuple._2.result.macro_eval_res.f1Measure.toString)
      table.addCell(tuple._2.result.micro_eval_res.f1Measure.toString)
    })


    section.add(table)

  }



  def printGlobalThresholdsPage(process_name:String,best_thresholds: Map[String, (Double, AggregatedEvaluationResult)], chapter: Chapter) = {
    val title = new Paragraph("Global Threshold optimization results" + process_name)
    val section = chapter.addSection(title)

    val table: PdfPTable = new PdfPTable(4);

    val c1: PdfPCell = new PdfPCell(new Phrase("Norm Technique"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c2: PdfPCell = new PdfPCell(new Phrase("threshold"));
    c2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c2);


    val c3: PdfPCell = new PdfPCell(new Phrase("F1 Macro"));
    c3.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c3);

    val c4: PdfPCell = new PdfPCell(new Phrase("F1 Micro"));
    c4.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c4);

    best_thresholds.foreach(tuple => {
      table.addCell(tuple._1)
      table.addCell(tuple._2._1.toString)
      table.addCell(tuple._2._2.macro_eval_res.f1Measure.toString)
      table.addCell(tuple._2._2.micro_eval_res.f1Measure.toString)
    })


    section.add(table)

  }


  def printAggregatedStatisticsPage(aggregated_result: AggregatedEvaluationResult): Element = {
    val anchor = new Anchor("Overall statistics");
    anchor.setName("Statistics");

    // Second parameter is the number of the chapter
    val catPart = new Chapter(new Paragraph(anchor), 1);
    val table: PdfPTable = new PdfPTable(2);

    val c1: PdfPCell = new PdfPCell(new Phrase("Name"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c2: PdfPCell = new PdfPCell(new Phrase("Micro Values"));
    c2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c2);

    table.addCell("Precision")
    table.addCell(aggregated_result.micro_eval_res.precision.toString)

    table.addCell("Recall")
    table.addCell(aggregated_result.micro_eval_res.recall.toString)

    table.addCell("F1 Measure")
    table.addCell(aggregated_result.micro_eval_res.f1Measure.toString)

    table.addCell("TP")
    table.addCell(aggregated_result.micro_eval_res.truePositives.toString)

    table.addCell("FP")
    table.addCell(aggregated_result.micro_eval_res.falsePositives.toString)

    table.addCell("FN")
    table.addCell(aggregated_result.micro_eval_res.FalseNegatives.toString)


    catPart.add(table)


    val table2: PdfPTable = new PdfPTable(2);

    table2.addCell(c1);

    val c2t2: PdfPCell = new PdfPCell(new Phrase("Macro Values"));
    c2t2.setHorizontalAlignment(Element.ALIGN_CENTER);

    table2.addCell(c2t2);

    table2.addCell("Precision")
    table2.addCell(aggregated_result.macro_eval_res.precision.toString)

    table2.addCell("Recall")
    table2.addCell(aggregated_result.macro_eval_res.recall.toString)

    table2.addCell("F1 Measure")
    table2.addCell(aggregated_result.macro_eval_res.f1Measure.toString)

    table2.addCell("TP")
    table2.addCell(aggregated_result.macro_eval_res.truePositives.toString)

    table2.addCell("FP")
    table2.addCell(aggregated_result.macro_eval_res.falsePositives.toString)

    table2.addCell("FN")
    table2.addCell(aggregated_result.macro_eval_res.FalseNegatives.toString)

    catPart.add(table2)

    catPart

  }

  def printPDFStatisticsPage(statistics: OutlierEvalStatisticsObject): Element = {
    val anchor = new Anchor("Statistics for " + statistics.ds_name);
    anchor.setName("Statistics");

    // Second parameter is the number of the chapter
    val catPart = new Chapter(new Paragraph(anchor), 1);


    val table: PdfPTable = new PdfPTable(2);

    val c1: PdfPCell = new PdfPCell(new Phrase("Name"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c2: PdfPCell = new PdfPCell(new Phrase("Value"));
    c2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c2);

    table.addCell("Standard Deviation")
    table.addCell(statistics.stdev + "")
    table.addCell("Mean")
    table.addCell(statistics.mean + "")
    table.addCell("p-value")
    table.addCell(statistics.p_value + "")
    table.addCell("Top 10 outlier scores")
    table.addCell(statistics.top_10_outlier_score.toString())
    table.addCell("Top Outlier Score")
    table.addCell(statistics.top_outlier_score.toString)




    catPart.add(table)

    catPart
  }

  def printPDFTopNEvaluatedPage(top_relations: Map[String, Map[String, Seq[(MatchRelation, Double, Boolean)]]], chapter:Chapter) = {

    val ds_name = top_relations.keys.head
    val result_by_technique = top_relations.values.head

    val section = chapter.addSection("Top Results")

    result_by_technique.foreach{case (norm_technique,results)=>{
      val subsection = section.addSection("Top "+results.size + " for " + norm_technique)
      val table: PdfPTable = new PdfPTable(3);

      val c1: PdfPCell = new PdfPCell(new Phrase("Relation"));
      c1.setHorizontalAlignment(Element.ALIGN_CENTER);
      table.addCell(c1);

      val c2: PdfPCell = new PdfPCell(new Phrase("Value"));
      c2.setHorizontalAlignment(Element.ALIGN_CENTER);
      table.addCell(c2);

      val c3: PdfPCell = new PdfPCell(new Phrase("Tp"));
      c3.setHorizontalAlignment(Element.ALIGN_CENTER);
      table.addCell(c3);


      for ((relation, score,tp) <- results) {
        table.addCell(relation.toString)
        table.addCell(score.toString)
        if(tp){
          table.addCell("X")
        }else {
          table.addCell("")
        }

      }

      subsection.add(table)

    }}

  }


  def printPDFTopNPage(top_relations: Seq[(MatchRelation, Double)], ds_name: String, ref_alignment: Alignment): Element = {
    val anchor = new Anchor("Top Elements for " + ds_name);
    anchor.setName("Top Elements for " + ds_name);

    // Second parameter is the number of the chapter
    val catPart = new Chapter(new Paragraph(anchor), 1);

    val table: PdfPTable = new PdfPTable(3);

    val c1: PdfPCell = new PdfPCell(new Phrase("Relation"));
    c1.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c1);

    val c2: PdfPCell = new PdfPCell(new Phrase("Value"));
    c2.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c2);

    val c3: PdfPCell = new PdfPCell(new Phrase("Tp"));
    c3.setHorizontalAlignment(Element.ALIGN_CENTER);
    table.addCell(c3);



    for ((relation, score) <- top_relations) {
      table.addCell(relation.toString)
      table.addCell(score.toString)

      val cell = new Cell(relation.left, relation.right, score, relation.relation, relation.owl_type)

      if (ref_alignment.correspondences.contains(cell)) {
        table.addCell("X")
      } else {
        table.addCell("")
      }

    }

    catPart.add(table)

    catPart
  }

  def printPDFChartPage(cb: PdfContentByte, width: Float, height: Float, chart_tuple: (JFreeChart, JFreeChart)) {
    val chart1: PdfTemplate = cb.createTemplate(width, height);
    val g2d1: Graphics2D = new PdfGraphics2D(chart1, width, height);
    val r2d1: Rectangle2D = new Rectangle2D.Double(0, 0, width, height);

    chart_tuple._1.draw(g2d1, r2d1)
    g2d1.dispose();
    cb.addTemplate(chart1, 0, height);


    val chart2: PdfTemplate = cb.createTemplate(width, height);
    val g2d2: Graphics2D = new PdfGraphics2D(chart2, width, height);
    val r2d2: Rectangle2D = new Rectangle2D.Double(0, 0, width, height);

    chart_tuple._2.draw(g2d2, r2d2)
    g2d2.dispose();
    cb.addTemplate(chart2, 0, 0);
  }

}

package de.unima.dws.oamatching.thesis

import java.awt.Graphics2D
import java.awt.geom.Rectangle2D
import java.io.FileOutputStream

import com.itextpdf.awt.PdfGraphics2D
import com.itextpdf.text._
import com.itextpdf.text.pdf._
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.pipeline.evaluation.EvaluationMatchingRunner
import org.jfree.chart.axis.LogAxis
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.data.statistics.HistogramDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
;

/**
 * Singleton to create some vizualizations of the data sets
 * Created by mueller on 18/02/15.
 */
object HistogramChartFactory {

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

    val eval_results = charts.map(tuples => tuples._2.eval_at_top_k).toList
    val aggregated_result = EvaluationMatchingRunner.computeAggregatedResults(eval_results)

    document.add(printAggregatedStatisticsPage(aggregated_result))
    document.newPage()

    document.add(printGlobalThresholdsPage(optimal_thresholds._1))
    document.newPage()

    document.add(printLocalThresholdsPage(optimal_thresholds._2, optimal_thresholds._3))
    document.newPage()

    printPrecisionRecallChart(precision_recall_data, cb, width - 20, height - 30, document)

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

  def printPrecisionRecallChart(precision_recall_data: Map[String, Seq[(Double, AggregatedEvaluationResult)]], cb: PdfContentByte, width: Float, height: Float, document: Document): Unit = {

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


  def printLocalThresholdsPage(agg_local_result: Map[String, AggregatedEvaluationResult], best_thresholds: Map[String, Seq[(Double, EvaluationResult)]]): Element = {
    val anchor = new Anchor("Local threshold optimization results");
    anchor.setName("Statistics");

    // Second parameter is the number of the chapter
    val catPart = new Chapter(new Paragraph(anchor), 1);
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

    catPart.add(table)

    catPart.newPage()

    best_thresholds.foreach { case (norm_technique, thresholds) => {
      catPart.add(printBestLocalThresholdTable(norm_technique, thresholds))
      catPart.newPage()
    }
    }
    catPart
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


  def printGlobalThresholdsPage(best_thresholds: Map[String, (Double, AggregatedEvaluationResult)]): Element = {
    val anchor = new Anchor("Threshold optimization results");
    anchor.setName("Statistics");

    // Second parameter is the number of the chapter
    val catPart = new Chapter(new Paragraph(anchor), 1);
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


    catPart.add(table)

    catPart
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

    table.addCell("Precision at top k")
    table.addCell(statistics.eval_at_top_k.precision.toString)

    table.addCell("Recall at top k")
    table.addCell(statistics.eval_at_top_k.recall.toString)

    table.addCell("F1 Measure at top k")
    table.addCell(statistics.eval_at_top_k.f1Measure.toString)

    table.addCell("tp at top k")
    table.addCell(statistics.eval_at_top_k.truePositives.toString)

    table.addCell("fp at top k")
    table.addCell(statistics.eval_at_top_k.falsePositives.toString)

    table.addCell("fn at top k")
    table.addCell(statistics.eval_at_top_k.FalseNegatives.toString)




    catPart.add(table)

    catPart
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

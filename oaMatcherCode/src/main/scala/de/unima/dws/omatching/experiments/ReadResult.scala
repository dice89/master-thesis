package de.unima.dws.omatching.experiments

import java.io.BufferedReader
import java.io.FileReader
import com.github.tototoshi.csv.CSVReader
import java.io.File
import de.unima.dws.omatching.pipeline.metaMatcher.AlignmentFromFile
import org.semanticweb.owl.align.AlignmentProcess
import scala.collection.convert.Wrappers.JEnumerationWrapper
import fr.inrialpes.exmo.align.parser.AlignmentParser
import org.semanticweb.owl.align.Evaluator
import org.semanticweb.owl.align.Alignment
import fr.inrialpes.exmo.align.impl.eval.PRecEvaluator
import java.util.Properties

object ReadResult extends App {

  var alignment: AlignmentProcess = new AlignmentFromFile("result.csv")

  alignment.align(null, null)

  alignment.cut(.2)
  val res = new JEnumerationWrapper(alignment.getElements());

  res.foreach(cell => println(cell.getObject1().toString() + " " + cell.getRelation().getRelation() + " " + cell.getObject2().toString() -> cell.getStrength()));

  var aparser: AlignmentParser = new AlignmentParser(0);
  var reference: Alignment = aparser.parse(new File("cmt-conference.rdf").toURI());

  var p: Properties = new Properties();
  var evaluator: Evaluator = new PRecEvaluator(reference, alignment);
  
  println(evaluator.eval(p))
}	
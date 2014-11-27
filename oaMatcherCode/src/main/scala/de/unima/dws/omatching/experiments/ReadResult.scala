package de.unima.dws.omatching.experiments

import java.io.BufferedReader
import java.io.FileReader
import com.github.tototoshi.csv.CSVReader
import java.io.File
import de.unima.dws.omatching.pipeline.metaMatcher.AlignmentFromFile
import org.semanticweb.owl.align.AlignmentProcess
import com.rapidminer.{Process => RProcess};
import scala.collection.convert.Wrappers.JEnumerationWrapper
import fr.inrialpes.exmo.align.parser.AlignmentParser
import org.semanticweb.owl.align.Evaluator
import org.semanticweb.owl.align.Alignment
import fr.inrialpes.exmo.align.impl.eval.PRecEvaluator
import java.util.Properties
import com.rapidminer.RapidMiner
import com.rapidminer.operator.io.CSVDataReader
import com.rapidminer.operator.io.CSVExampleSetWriter

object ReadResult extends App {

	/*var alignment: AlignmentProcess = new AlignmentFromFile("result.csv")

  alignment.align(null, null)

  alignment.cut(.2)
  val res = new JEnumerationWrapper(alignment.getElements());

  res.foreach(cell => println(cell.getObject1().toString() + " " + cell.getRelation().getRelation() + " " + cell.getObject2().toString() -> cell.getStrength()));

  var aparser: AlignmentParser = new AlignmentParser(0);
  var reference: Alignment = aparser.parse(new File("cmt-conference.rdf").toURI());

  var p: Properties = new Properties();
  var evaluator: Evaluator = new PRecEvaluator(reference, alignment);
  
  println(evaluator.eval(p))*/
  
  RapidMiner.setExecutionMode(RapidMiner.ExecutionMode.COMMAND_LINE);
  RapidMiner.init();
  
  var process:RProcess = new RProcess(new File("/Users/mueller/Documents/master-thesis/RapidminerRepo/oacode.rmp"));
  var f:File= new File("test2.csv");
    var f_res:File= new File("result2.csv");
  println(f.getAbsolutePath());
  process.getOperator("Input").setParameter( CSVDataReader.PARAMETER_CSV_FILE,  f.getAbsolutePath())
  
  process.getOperator("Output").setParameter(  CSVExampleSetWriter.PARAMETER_CSV_FILE,  f_res.getAbsolutePath())

  process.run();
  
}	
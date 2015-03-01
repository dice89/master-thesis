package de.unima.dws.oamatching.pipeline.evaluation
import java.io.File

import de.unima.dws.oamatching.alex.XMLTest
import de.unima.dws.oamatching.core.{AlignmentParser, Alignment, OntologyLoader}
import de.unima.dws.oamatching.pipeline.MatchingProblem

/**
 * Trait that implements the OAEI Challenge Dataset parsing
 *
 * Created by mueller on 01/03/15.
 */
trait EvaluationDataSetParser {

  /**
   * Parses the conference dataset from OAEI challenge
   * @param path_to_folder
   * @return
   */
  def parseConference(path_to_folder: String): Seq[EvaluationMatchingTask] = {
    val folder: File = new File(path_to_folder + File.separator + "reference-alignment/")

    val problems = for (ref_align_file <- folder.listFiles(new RdfFileFilter)) yield {

      val ontos: List[String] = ref_align_file.getName().split("-").toList
      val name_onto1: String = path_to_folder + File.separator + ontos(0).replaceAll("-", "") + ".owl"
      val name_onto2: String = path_to_folder + File.separator + ontos(1).replaceAll("-", "").replaceAll(".rdf", "") + ".owl"


      val onto1 = OntologyLoader.load(name_onto1)
      val onto2 = OntologyLoader.load(name_onto2)

      //parse alignments
      val reference: Alignment = AlignmentParser.parseRDFWithOntos(ref_align_file.getAbsolutePath(),name_onto1,name_onto2)
      val name: String = ref_align_file.getName().dropRight(4)
      val matching_problem = MatchingProblem(onto1, onto2, name)

      EvaluationMatchingTask(matching_problem, reference)
    }

    problems
  }


  def parseBenchmarks(path_to_folder:String):Seq[EvaluationMatchingTask] = {

    val left_name:String = "101"
    val onto_left_file:File = new File(path_to_folder +File.separator + left_name + File.separator +"onto.rdf")
    val onto_left = OntologyLoader.load(onto_left_file)

    val folder: File = new File(path_to_folder)

    val problems: Array[EvaluationMatchingTask] = for(benchmark_folders <- folder.listFiles(new FolderFilter)) yield {
      val onto_right_path:String =  benchmark_folders.getAbsolutePath + File.separator +"onto.rdf"
      val onto_right = OntologyLoader.load(onto_left_file)

      val right_name:String = benchmark_folders.getName

      val ref_align_path:String =  benchmark_folders.getAbsolutePath + File.separator +"refalign.rdf"

      val reference_alignment:Alignment =  AlignmentParser.parseRDFWithOntos(ref_align_path,onto_left_file.getAbsolutePath,onto_right_path)


      val name:String = left_name+"_"+right_name

      val matching_problem = MatchingProblem(onto_left, onto_right, name)

      EvaluationMatchingTask(matching_problem, reference_alignment)
    }

    problems
  }

  def parseSingle(f_onto1: String, f_onto2: String, f_reference: String): EvaluationMatchingTask = {

    //name of the problem is the name of the rdf file alignment without .rdf extension
    val name: String = f_reference.dropRight(4)
    val onto1 = OntologyLoader.load(f_onto1)
    val onto2 = OntologyLoader.load(f_onto2)
    val reference: Alignment = AlignmentParser.parseRDF(f_reference)

    val matching_problem = MatchingProblem(onto1, onto2, name)

    EvaluationMatchingTask(matching_problem, reference)
  }


  def getListofProblemMatchingTasks(problems:Seq[EvaluationMatchingTask], path_to_matchings:String): List[(EvaluationMatchingTask, File)] = {

    //read matchings folder
    val file: File = new File(path_to_matchings)
    val list_of_raw_matchings = file.listFiles().toList

    //build pairs of matchings to reference alignment
    val problem_matching_pairs =  problems.map(problem =>{
      val name = problem.matching_problem.name

      list_of_raw_matchings.map(raw_file => {
        val id_part = raw_file.getName.split("_")(0)

        if(id_part.equals(name)){
          Option(problem,raw_file)
        }else {
          Option.empty
        }
      }).filter(_.isDefined).head.get
    })

    problem_matching_pairs.toList
  }



}

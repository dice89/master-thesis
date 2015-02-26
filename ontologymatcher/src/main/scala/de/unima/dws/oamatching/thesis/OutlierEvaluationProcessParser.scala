package de.unima.dws.oamatching.thesis

import java.io.File

import de.unima.dws.oamatching.core.EvaluationResult
import de.unima.dws.oamatching.pipeline.util.RmpFileFilter

/**
 * Created by mueller on 24/02/15.
 */

  
case class OutlierEvaluationProcessesBySepartation(separated:OutlierEvaluationProcessesByPreprocess, non_separated:OutlierEvaluationProcessesByPreprocess)
case class OutlierEvaluationProcessesByPreprocess(pca_variance:List[String], pca_fixed:List[String], remove_correlated:List[String])

trait OutlierEvaluationProcessParser {
  
  def parseOutlierEvaluationProcesses(base_dir:String,filter:String):OutlierEvaluationProcessesBySepartation = {
    var separated_dir = base_dir+"/separated"
    var non_separated_dir = base_dir+"/non_separated"

    val separated_files = parseOutlierEvaluationProcessesByPreprocesses(separated_dir, filter)
    val non_separated_files = parseOutlierEvaluationProcessesByPreprocesses(non_separated_dir,filter)

    OutlierEvaluationProcessesBySepartation(separated_files,non_separated_files)
  }

  def parseOutlierEvaluationProcessesByPreprocesses(base_dir:String, filter:String):OutlierEvaluationProcessesByPreprocess = {

    val pca_variance_dir = base_dir +"/pre_pro_pca_v"
    val pca_fixed_dir = base_dir +"/pre_pro_pca_f"
    val remove_corr_dir = base_dir +"/pre_pro_remove_corr"

    val pca_var_files = parseOutlierEvaluationProcessesForOneFolder(pca_variance_dir,filter)
    val pca_fixed_files = parseOutlierEvaluationProcessesForOneFolder(pca_fixed_dir,filter)
    val remove_corr_files = parseOutlierEvaluationProcessesForOneFolder(remove_corr_dir,filter)

    OutlierEvaluationProcessesByPreprocess(pca_var_files,pca_fixed_files,remove_corr_files)
  }

  def parseOutlierEvaluationProcessesForOneFolder(base_dir:String, filter:String):List[String] = {
    val folder = new File(base_dir)

    folder.listFiles(new RmpFileFilter(filter)).map(file => file.getAbsolutePath).toList
  }
}

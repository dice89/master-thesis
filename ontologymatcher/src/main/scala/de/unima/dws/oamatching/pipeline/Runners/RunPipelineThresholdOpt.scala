package de.unima.dws.oamatching.pipeline.Runners

import de.unima.dws.oamatching.pipeline.evaluation.EvaluationMatchingRunner
import de.unima.dws.oamatching.pipeline.optimize.ParameterOptimizer

/**
 * Created by mueller on 07/02/15.
 */
object RunPipelineThresholdOpt extends App{
  val problems = EvaluationMatchingRunner.parseConference("ontos/2014/conference")

  ParameterOptimizer.optimizeAdvancedPipeline("conference",problems,ParameterOptimizer.getDoubleGrid(0.1,0.95,15))
}

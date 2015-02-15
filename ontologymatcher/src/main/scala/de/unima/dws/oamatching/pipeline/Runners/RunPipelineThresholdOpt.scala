package de.unima.dws.oamatching.pipeline.Runners

import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.Runner
import de.unima.dws.oamatching.pipeline.evaluation.EvaluationMatchingRunner
import de.unima.dws.oamatching.pipeline.optimize.ParameterOptimizer

/**
 * Created by mueller on 07/02/15.
 */
object RunPipelineThresholdOpt extends App{
  MatcherRegistry.initLargeScale()
  val problems = EvaluationMatchingRunner.parseConference("ontos/2014/conference")
  val config =  Runner.parseRunConfig()

  ParameterOptimizer.optimizeThresholdPipeline(config.matching_pipline)("conference",problems,ParameterOptimizer.getDoubleGrid(0.4,0.8,4))

}
package de.unima.dws.oamatching.pipeline.Runners

import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.pipeline.Runner
import de.unima.dws.oamatching.pipeline.util.MetaDataMgmt

/**
 * Created by mueller on 07/02/15.
 */
object RunRound extends App{
  val config = Runner.parseRunConfig()


  val threshold = MetaDataMgmt.getThreshold("Conference","pipeline").getOrElse(0.7)
  Runner.runRound(config)
}


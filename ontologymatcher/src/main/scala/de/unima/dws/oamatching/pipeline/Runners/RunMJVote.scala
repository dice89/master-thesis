package de.unima.dws.oamatching.pipeline.Runners

import de.unima.dws.oamatching.pipeline.{SingleMatcherPipeline, MatchingPipelineCore}

/**
 * Created by mueller on 13/04/15.
 */
object RunMJVote extends  App{

  SingleMatcherPipeline.calcMJVoteBaseLine()
}

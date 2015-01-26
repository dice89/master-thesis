package de.unima.dws.omatching.pipeline

import com.redis.RedisClient

import de.unima.dws.oamatching.config.Config
import de.uniman.dws.oamatching.logging.ResultLogger

object ThresholdOptimizationPlatform {
/*  //init redis client
  val redis_client = new RedisClient(Config.REDIS_HOST, Config.REDIS_PORT)
  redis_client.connect

  MatcherRegistry.init

  def main(args: Array[String]): Unit = {
    //TODO parameter handling
    val problems = null //Pipeline.parse_conference_2014("ontos/2014/conference")
    optimizeThresholdBaseMachterGlobal(problems, 10, "conference")
  }

  def optimizeThresholdBaseMachterGlobal(problems: Seq[MatchingProblem], steps: Int, dataset_name: String) = {
    for (
      matcher <- MatcherRegistry.matcher_by_name.keys;
      if (getThreshold(matcher, dataset_name).isEmpty)
    ) {
      optimizeThresholdSingleBaseMatcherGlobal(problems, matcher, steps, dataset_name)
    }
  }

  /**
   * Optimizing the threshold for a single matching problem
   * @param matcher_name
   * @param steps
   * @param problem
   * @param dataset_name
   * @return
   */
  def optimizeThresholdBaseMatcherLocal(matcher_name: String, steps: Int, problem: MatchingProblem, dataset_name: String): Double = {
    val stepsize: Double = 1.0 / steps.asInstanceOf[Double]

    val results = for (index <- 0 until steps) yield {
      val threshold: Double = 1.0 - (stepsize * index)
      (threshold, MatcherRegistry.matchSingle(problem, matcher_name, threshold)._1)
    }
    val best_result = results.reduceLeft((B, A) => {

      if (B._2.fmeasure > A._2.fmeasure) {
        B
      } else {
        A
      }
    })
    // best threshold
    best_result._1
  }

  /**
   * Optimizes the threshold of a given matcher for a given set of matching problems, in n steps, so basically a grid search is performed
   * @param problems
   * @param matcher_name
   * @param steps
   * @param dataset_name
   * @return
   */
  def optimizeThresholdSingleBaseMatcherGlobal(problems: Seq[MatchingProblem], matcher_name: String, steps: Int, dataset_name: String): Double = {
    val stepsize: Double = 1.0 / steps.asInstanceOf[Double]

    val results = for (
      index <- 0 until steps;
      threshold <- Option[Double](1.0 - (stepsize * index));
      if (threshold > 0.1)
    ) yield {
      println("Start Matcher " + matcher_name)
      val res = (threshold, MatcherRegistry.matchProblemsWithOneMatcherOptimizeOnly(problems, matcher_name, dataset_name, threshold))

      println("Matcher " + matcher_name + " " + res)
      Option(res)
    }
    val best_result = results.reduceLeft((B, A) => {
      if (B.isDefined && A.isDefined) {
        if (B.get._2.fmeasure > A.get._2.fmeasure) {
          B
        } else {
          A
        }
      } else {
        if (B.isDefined && A.isEmpty) {
          B
        } else {
          A
        }
      }

    })

    ResultLogger.log("Best Threshold for: " + matcher_name + " for dataset " + dataset_name + " is " + best_result.get._1)

    setThreshold(matcher_name, dataset_name, best_result.get._1)
    best_result.get._1
  }
  
  def optimizeMetaMatcherLocally(problems:Seq[MatchingProblem]):Unit = {
    
    
  }
  
  
  

  /**
   * Set Locally Optimized Threshold
   * @param matcher_name
   * @param dataset_name
   * @param matching_problem_name
   * @param threshold
   */
  def setThreshold(matcher_name: String, dataset_name: String, matching_problem_name: String, threshold: Double): Unit = {
    redis_client.set(dataset_name + "-" + matcher_name + "-" + matching_problem_name, threshold)
  }

  /**
   * Get Locally optimized Threshold
   * @param matcher_name
   * @param dataset_name
   * @param matching_problem_name
   * @return
   */
  def getThreshold(matcher_name: String, dataset_name: String, matching_problem_name: String): Option[Double] = {

    redis_client.get[Double](dataset_name + "-" + matcher_name + "-" + matching_problem_name)(com.redis.serialization.Format.default, com.redis.serialization.Parse.Implicits.parseDouble)

  }
  /**
   * Sets the globally optimized Threshold
   * @param matcher_name
   * @param dataset_name
   * @param threshold
   */
  def setThreshold(matcher_name: String, dataset_name: String, threshold: Double): Unit = {
    redis_client.set(dataset_name + "-" + matcher_name, threshold + "")
  }

  /**
   * Gets the globally optimized threshold for a specific matcher and dataset
   * @param matcher_name
   * @param dataset_name
   * @return
   */
  def getThreshold(matcher_name: String, dataset_name: String): Option[Double] = {

    redis_client.get[Double](dataset_name + "-" + matcher_name)(com.redis.serialization.Format.default, com.redis.serialization.Parse.Implicits.parseDouble)

    //TODO
  }
  */
}
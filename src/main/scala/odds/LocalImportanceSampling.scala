package odds

import scala.collection.mutable

/**
 * Importance sampling with depth-bound look-ahead.
 *
 * This trait implements importance sampling as described in "Embedded
 * Probabilistic Programming" by Oleg Kiselyov and Chung-chieh Shan.
 */
trait LocalImportanceSampling extends DelayedChoiceIntf {

  /**
   * Approximate the distribution defined by a probabilistic
   * computation.
   *
   * @param samples the number of samples used to approximate the
   *        distribution.
   * @param depth the look-ahead depth used during exploration.
   *        `0` corresponds to rejection sampling.
   * @param error the maximum upper bound on the total weight of
   *        unexplored values.
   * @param x the random variable to reify.
   * @return an approximation of the distribution over the values
   *         of this random variable.
   */
  def sample[A](samples: Int, depth: Int = 1, error: Prob = 0.0)(
    x: Rand[A]): Dist[A] = {
    var d: Dist[A] = dist()
    var solCount = 0;
    var runCount = 0;
    while (runCount < samples) {
      val sd = sample1(x, depth, error)
      d ++= sd
      solCount += sd.size
      runCount += 1
    }
    println("importance sampler: " + runCount + " samples, " + solCount + " solutions.")
    d
  }

  /** Generate a single "sample" for importance sampling. */
  def sample1[A](x: RandVar[A], depth: Int, error: Prob): Dist[A] = {

    // bounded look-ahead exploration of search tree
    def lookAhead(r: ExploreRes[A], depth: Int): ExploreRes[A] =
      if (depth <= 0) r
      else {
        val ExploreRes(sls, bts) = r
        val z = ExploreRes(sls, dist())
        (z /: bts) { (sb, tp) =>
          val (t, p) = tp
          val ExploreRes(slsAcc, btsAcc) = sb
          val ExploreRes(sls, bts) = lookAhead(t(p), depth - 1)
          ExploreRes(slsAcc ++ sls, btsAcc ++ bts)
        }
      }

    var ExploreRes(solutions, branches) = lookAhead(explore(x), depth)

    // Keep exploring candidate branches until none remain.
    while (!branches.isEmpty &&
      (branches.totalWeight != 0.0) &&  // Don't explore infinities
      (branches.totalWeight >= error)) {

      // Pick a branch thunk at random
      val t = branches.nextSample

      // Continue exploring the search tree with look-ahead `depth`.
      val er = lookAhead(t(branches.totalWeight), depth)
      solutions ++= er.solutions
      branches = er.branches
    }
    solutions
  }
}

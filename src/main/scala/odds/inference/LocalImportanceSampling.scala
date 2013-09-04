package odds
package inference

import scala.collection.mutable

/**
 * Importance sampling with depth-bound look-ahead.
 *
 * This trait implements importance sampling as described in "Embedded
 * Probabilistic Programming" by Oleg Kiselyov and Chung-chieh Shan.
 */
trait LocalImportanceSampling extends DelayedChoiceIntf {
  this: OddsIntf =>

  /** Concrete probability monad type class. */
  implicit object Rand extends DelayedChoiceMonad

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
  def sample[A](
    samples: Int, depth: Int = 1, initDepth: Int = 3, error: Prob = 0.0)(
    x: Rand[A]): Dist[A] = {

    // ASCII art progress bar
    def progressBar(count: Int, total: Int): String = {
      val bar =
        if (count >= total) "[" + "=" * 32 + ">] "
        else {
          val b: Int = count * 32 / total
          "[" + "=" * b + ">" + " " * (32 - b) + "] "
        }
      bar + count + "/" + total
    }

    // bounded look-ahead exploration of search tree
    def lookAhead(r: ExploreRes[A], depth: Int): ExploreRes[A] =
      if (depth <= 0) r
      else {
        val ExploreRes(sls, bts, i) = r
        val z = ExploreRes(sls, dist(), i)
        (z /: bts) { (sb, tp) =>
          val (t, p) = tp
          val ExploreRes(slsAcc, btsAcc, i) = sb
          val ExploreRes(sls, bts, _) = lookAhead(t(p), depth - 1)
          ExploreRes(slsAcc ++ sls, btsAcc ++ bts, i)
        }
      }

    // Generate a single "sample"
    def sample1(r: ExploreRes[A], depth: Int, error: Prob): Dist[A] = {

      var ExploreRes(solutions, branches, _) = r

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
      consolidate(solutions)
    }

    val initEr = lookAhead(explore(x), initDepth)
    var d: Dist[A] = dist()
    var solCount = 0;
    var runCount = 0;
    while (runCount < samples) {
      if (runCount % (samples / 1024 + 1) == 0)
        print("\r" + progressBar(runCount, samples))
      val sd = sample1(initEr, depth, error)
      d ++= sd
      solCount += sd.size
      runCount += 1
    }
    println("\r" + progressBar(runCount, samples))
    println("importance sampler: " + runCount + " samples, " + solCount +
      " solutions.")
    scale(1.0 / runCount, consolidate(d))
  }
}

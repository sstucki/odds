package odds
package inference

/**
 * Depth-bound inference for the ODDS language.
 *
 * This trait provides a depth-bound inference algorithm based on a
 * depth-first, iterative-deepening traversal of the search tree
 * defined by a probabilistic computation.
 */
trait DepthBoundInference extends DelayedChoiceIntf {
  this: OddsLang =>

  /** Concrete probability monad type class. */
  implicit object probMonad extends DelayedChoiceMonad

  /**
   * Reify a random variable representing a probabilistic computation.
   *
   * This method explores the search tree associated with the
   * probabilistic computation represented by this random variable
   * until either
   *
   *  1. the number of explored values of the distribution is
   *     greater than `solutions`, or
   *
   *  2. the upper bound on the total weight of unexplored values is
   *     lower than `error`.
   *
   * @param solutions the minimum number of explored values in the
   *        resulting distribution.
   * @param error the maximum upper bound on the total weight of
   *        unexplored values.
   * @return a pair `(d, w)`, with `d` the distribution over the
   *         explored values of this random variable, and `w` the
   *         sum of the weight of the unexplored values of this
   *         random variable.
   * @param x the random variable to reify.
   */
  def reify[A](solutions: Int, error: Prob = 0.0)(
    x: Rand[A]): (Dist[A], Prob) = {

    // bounded depth-first traversal of search tree
    def bdft(r: ExploreRes[A], depth: Int): (Dist[A], Prob) = {
      val ExploreRes(sls, bts, _) = r
      if (depth <= 0) (sls, bts.map(_._2).sum)
      else ((sls, 0.0) /: bts) { (se, tp) =>
        val (t, p) = tp
        val (slsAcc, eAcc) = se
        val (sls, e) = bdft(t(p), depth - 1)
        (slsAcc ++ sls, eAcc + e)
      }
    }

    var d: Dist[A] = dist()
    var err: Prob = error + 1
    var depth: Int = 1
    while ((d.size < solutions) && (err > error)) {
      print("depth " + depth + "... ")
      val (d1, err1) = bdft(explore(x), depth)
      d = consolidate(d1)
      err = err1
      println("err = " + err + " (thsld = " + error + "), d.size = " + d.size)
      depth += 1
    }
    (d, err)
  }
}

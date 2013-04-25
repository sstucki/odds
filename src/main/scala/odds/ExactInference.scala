package odds

/** Simple, exact inference for the ODDS language. */
trait ExactInference extends DelayedChoiceIntf {

  /**
   * Reify a random variable representing a probabilistic computation.
   *
   * @return the distribution over the values of this random variable.
   */
  def reify[A](x: RandVar[A]): Dist[A] = {

    // depth-first traversal of search tree
    def dft(r: ExploreRes[A]): Dist[A] = {
      val ExploreRes(sls, bts) = r
      (sls /: bts) { (sls, tp) =>
        val (t, p) = tp
        sls ++ dft(t(p))
      }
    }

    consolidate(dft(explore(x)))
  }
}

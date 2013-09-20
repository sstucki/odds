package ch.epfl.lamp.odds
package inference

import internal.{ DelayedChoiceIntf, OddsIntf }

/** Simple, exact inference for the ODDS language. */
trait ExactInference extends DelayedChoiceIntf {
  this: OddsIntf =>

  /** Concrete probability monad type class. */
  implicit object Rand extends DelayedChoiceMonad

  /**
   * Reify a random variable representing a probabilistic computation.
   *
   * @return the distribution over the values of this random variable.
   */
  def reify[A](x: RandVar[A]): Dist[A] = {

    // depth-first traversal of search tree
    def dft(r: ExploreRes[A]): Dist[A] = {
      val ExploreRes(sls, bts, _) = r
      (sls /: bts) { (sls, tp) =>
        val (t, p) = tp
        sls ++ dft(t(p))
      }
    }

    println("running exact inference...")
    consolidate(dft(explore(x)))
  }
}

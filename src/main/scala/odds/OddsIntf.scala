package odds

import functors.ProbMonad


/** ODDS Language interface. */
trait OddsIntf extends DistIntf {

  import macros._

  /** Abstract random variable type. */
  type Rand[+A] <: RandIntf[A]

  /** Abstract distribution type. */
  //type Dist[+A]

  /**
   * Generic operations on random variables.
   *
   * This trait provides the public interface of the [[Rand]] type.
   * It must be extended by any concrete instance of the random
   * variable type.
   */
  trait RandIntf[+A] extends MonadLifter[Rand] with RandMacroOps[Rand] {
    this: Rand[A] =>

    /**
     * Condition this random variable on an observation.
     *
     * Use case:
     *
     * {{{
     *     val coin1 = flip(0.5)
     *     val coin2 = flip(0.5)
     *     val both  = coin1 && coin2
     *
     *     // conditional probability: P(both | coin1) = P(coin2)
     *     both when coin1
     * }}}
     *
     * @param cond the observation (random variable) to condition on.
     * @return the conditional probability of `this` given `cond`,
     *         i.e. `P(this | cond)`
     */
    def when(cond: Rand[Boolean]): Rand[A] = probMonad.bind {
      c: Boolean => if (c) this else probMonad.zero
    } (cond)
  }

  /**
   * Abstract type class instance for `Rand` type.
   *
   * This type class provides the internal interface of the
   * probability monad.  Any inference algorithm must be provided a
   * concrete instance of it.
   */
  implicit val probMonad: ProbMonad[Rand, Dist]
}

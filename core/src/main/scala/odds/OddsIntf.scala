package ch.epfl.lamp.odds

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
     * FIXME: Make this work with deep conditions.
     *
     * @param cond the observation (random variable) to condition on.
     * @return the conditional probability of `this` given `cond`,
     *         i.e. `P(this | cond)`
     */
    def when(cond: Rand[Boolean]): Rand[A] = Rand.bind(cond) {
      c: Boolean => if (c) this else Rand.zero
    }
  }

  /**
   * Internal interface of the probability monad.
   *
   * This is the type class of the probability monad used by a
   * particular inference algorithm.
   */
  trait RandInternalIntf extends ProbMonad[Rand, Dist] {

    /**
     * Factory method for creating a unit instance of type `Rand[A]`
     * from a value of type `A`.
     */
    def apply[A](x: A): Rand[A] = unit(x)
  }

  /**
   * Abstract type class instance for the `Rand` type.
   *
   * This type class provides the internal interface of the
   * probability monad.  Any inference algorithm must be provided a
   * concrete instance of it.
   */
  implicit val Rand: RandInternalIntf
}

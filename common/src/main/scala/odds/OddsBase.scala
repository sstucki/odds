package ch.epfl.lamp.odds
package internal

import functors.ProbMonad


/** Base trait for ODDS Language interface. */
trait OddsBase {

  /** Abstract random variable type. */
  type Rand[+A] <: RandBase[A]

  /** Abstract distribution type. */
  type Dist[+A]

  /** Base trait for random variables. */
  trait RandBase[+_]

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

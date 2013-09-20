package ch.epfl.lamp.odds
package internal

/** ODDS Language interface. */
trait OddsIntf extends OddsBase with DistIntf {

  import macros._

  /** Abstract random variable type. */
  type Rand[+A] <: RandIntf[A]

  /**
   * Generic operations on random variables.
   *
   * This trait provides the public interface of the [[Rand]] type.
   * It must be extended by any concrete instance of the random
   * variable type.
   */
  trait RandIntf[+A]
      extends RandBase[A]
      with MonadLifter[Rand]
      with RandMacroOps[Rand]
}

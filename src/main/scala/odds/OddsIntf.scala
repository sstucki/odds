package odds

import functors.ProbMonad


/** ODDS Language interface. */
trait OddsIntf extends DistIntf {
  this: OddsLang =>

  /** Abstract random variable type. */
  type Rand[+A]

  /** Abstract distribution type. */
  //type Dist[+A]

  /**
   * Abstract type class instance for `Rand` type.
   *
   * This type class provides the internal interface of the
   * probability monad.  Any inference algorithm must be provided a
   * concrete instance of it.
   */
  implicit val probMonad: ProbMonad[Rand, Dist]
}

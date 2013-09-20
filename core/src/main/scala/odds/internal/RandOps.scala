package ch.epfl.lamp.odds
package internal

import language.implicitConversions

/**
 * Stochastic primitives.
 *
 * This trait provides the basic operations defined on random
 * variables.
 */
trait RandOps { this: OddsIntf =>

  import Rand._

  import Rand._

  /**
   * Make a (discrete) probabilistic choice.
   *
   * @tparam A the type of the support of the random variable
   *         returned by this function.
   * @param xs A sequence of value-weight pairs representing the
   *        discrete distribution to chose from.
   * @return a random variable representing the outcome of the choice.
   */
  @inline final def choose[A](xs: (A, Prob)*): Rand[A] =
    Rand.choose(Dist(xs: _*))

  /**
   * Make a probabilistic choice.
   *
   * @tparam A the type of the support of the random variable
   *         returned by this function.
   * @param xs A distribution to chose from.
   * @return a random variable representing the outcome of the choice.
   */
  @inline final def choose[A](xs: Dist[A]): Rand[A] = Rand.choose(xs)

  /** Make a (discrete) probabilistic choice. */
  @deprecated("use `choose` instead", "2013-08-19")
  @inline final def choice[A, M[A]](xs: (A, Prob)*) = choose(xs: _*)

  @inline implicit final def always[A](x: A): Rand[A] = unit(x)

  @inline final def never[A]: Rand[A] = zero

  @inline implicit final def flatten[A](mmx: Rand[Rand[A]]): Rand[A] =
    join(mmx)

  @inline def flip(p: Double): Rand[Boolean] =
    choose(true -> p, false -> (1-p))
  @inline def flip(rp: Rand[Double]): Rand[Rand[Boolean]] = fmap(rp)(flip _)

  def uniform[A](xs: A*): Rand[A] = if (xs.isEmpty) never else {
    val p = 1.0 / xs.length
    choose(xs.map((_, p)): _*)
  }
  @inline def uniform[A](rxs: Rand[Seq[A]]): Rand[Rand[A]] =
    fmap(rxs)(xs => uniform(xs: _*))

  /** Condition a probabilistic choice on a Boolean random variable. */
  @inline def cond[A](rx: => Rand[A], rc: Rand[Boolean]): Rand[A] = bind(rc) {
    c: Boolean => if (c) rx else zero
  }
}

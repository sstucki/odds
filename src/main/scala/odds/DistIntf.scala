package odds

import scala.language.implicitConversions


/** Abstract distribution type and associated operations. */
trait DistIntf {
  this: OddsIntf =>

  /** Distribution type. */
  type Dist[+A]

  /**
   * We want to be able to view discrete distributions as iterables of
   * value-weight pairs.
   */
  implicit def distToIterable[A](d: Dist[A]): Iterable[(A, Prob)]

  /** Build a distribution from a sequence of value-weight pairs. */
  def dist[A](xs: (A, Prob)*): Dist[A]

  /** Scale the weights of a distribution by a given value. */
  def scale[A](w: Prob, xs: Dist[A]): Dist[A]

  /**
   * Flatten a distribution by accumulating weights of identical
   * support values.
   */
  def consolidate[A](xs: Dist[A]): Dist[A]

  /**
   * Normalize a distribution.
   *
   * If the total weight of the distribution is zero, the function
   * returns the empty distribution.
   */
  def normalize[A](xs: Dist[A]): Dist[A]
}

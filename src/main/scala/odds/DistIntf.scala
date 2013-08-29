package odds

import scala.language.implicitConversions


/** Abstract distribution type and associated operations. */
trait DistIntf {

  /**
   * Probability weight type.
   *
   * Note: this need not be normalized to lie within `[0, 1]`.
   */
  type Prob = Double

  /** Distribution type. */
  type Dist[+A] <: Iterable[(A, Prob)]

  /**
   * Build a discrete distribution from a sequence of value-weight
   * pairs.
   */
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

  /** Companion object. */
  object Dist {

    /** Build a distribution from a sequence of value-weight pairs. */
    def apply[A](xs: (A, Prob)*): Dist[A] = dist(xs: _*)

    /** Extract a sequence of value-weight pairs from a distribution. */
    def unapplySeq[A](d: Dist[A]) = Some(d.toSeq)
  }
}

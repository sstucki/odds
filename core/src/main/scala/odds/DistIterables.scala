package ch.epfl.lamp.odds

import scala.language.implicitConversions


/** Simple, Iterable-based distributions. */
trait DistIterables extends DistIntf {
  this: OddsIntf =>

  /**
   * Distribution type.
   *
   * Discrete distributions are just iterables of value-weight pairs.
   */
  type Dist[+A] = Iterable[(A, Prob)]

  /** Build a distribution from a sequence of value-weight pairs. */
  def dist[A](xs: (A, Prob)*): Dist[A] = Iterable(xs: _*)

  /** Scale the weights of a distribution by a given value. */
  def scale[A](w: Prob, xs: Dist[A]): Dist[A] = {
    xs map { case (x, p) => (x, p * w) }
  }

  /**
   * Compress a distribution by accumulating weights of identical
   * support values.
   */
  def consolidate[A](xs: Dist[A]): Dist[A] = {
    xs.filter(_._2 > 0).groupBy(_._1) map {
      case (x, ps) => (x, ps.map(_._2).sum)
    }
  }

  /**
   * Normalize a distribution.
   *
   * If the total weight of the distribution is zero, the function
   * returns the empty distribution.
   */
  def normalize[A](xs: Dist[A]): Dist[A] = if (xs.isEmpty) xs else {
    val totalWeight = xs.map(_._2).sum
    if (totalWeight == 0) Iterable()
    else scale(1 / totalWeight, xs)
  }
}

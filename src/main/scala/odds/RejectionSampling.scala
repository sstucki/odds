package odds

import scala.collection.mutable

/** Simple, rejection sampler for the ODDS language. */
trait RejectionSampling extends OddsIntf with DistIterables {

  val prng = new java.util.Random

  type Rand[+A] = RandVar[A]

  final case class RandVar[+A](v: Option[A]) extends RandIntf[A] {
    def flatMap[B](f: A => Rand[B]): Rand[B] = RandVar(v.flatMap(f(_).v))
    def orElse[B >: A](that: Rand[B]): Rand[B] = RandVar(this.v orElse that.v)
  }

  def choice[A](xs: (A, Prob)*): Rand[A] = if (xs.isEmpty) RandVar(None) else {
    val (vals, weights) = xs.unzip
    val idx = DistTree(weights, prng).nextRandom._1
    RandVar(Some(vals(idx)))
  }

  def geometric[A](p: Double): Rand[Int] = {
    val s = prng.nextDouble

    //here is some imperative code
    var acc = 0.0
    var k = 1
    while (acc < s) {
      k += 1
      acc += Math.pow((1-p),(k-1))*p
    }
    RandVar(Some(k))
  }

  /**
   * Approximate the distribution defined by a probabilistic
   * computation.
   *
   * @param samples the number of samples used to approximate the
   *        distribution.
   * @return an approximation of the distribution over the values
   *         of this random variable.
   */
  def sample[A](samples: Int)(x: => Rand[A]): Dist[A] = {
    val distMap = new mutable.HashMap[A, Prob]()
    for (_ <- 1 to samples; v <- x.v) {
      distMap(v) = distMap.getOrElse(v, 0.0) + 1.0
    }
    scale(1.0 / samples, distMap)
  }
}

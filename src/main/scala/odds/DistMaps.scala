package odds

import scala.language.implicitConversions


/** Distributions implemented as maps. */
trait DistMaps extends DistIntf {
  this: OddsIntf =>

  import scala.collection.{GenTraversableOnce, Iterable, IterableLike}
  import scala.collection.immutable.Map
  import scala.collection.mutable.Builder

  type Dist[+A] = DistMap[A]

  val prng = new java.util.Random  // Where should this go?

  /**
   * Map-based distributions.
   *
   * Discrete distributions just wrap maps from values to weights.
   *
   * Note: we must restrict access to the encapsulated map in order to
   * keep [[DistMap]] covariant in it's type parameter `A`-
   *
   * @tparam A the value type of this distribution.
   */
  class DistMap[+A](private[this] val _dist: Map[A, Prob])
      extends Iterable[(A, Prob)]
      with IterableLike[(A, Prob), DistMap[A]] {

    import DistMap._

    // Support and distribution tree -- only constructed if needed
    protected[this] lazy val (_supportSeq, _distTree) =
      if (_dist.isEmpty) (Nil, null) else {
        val (ks, vs) = _dist.toArray.unzip
        (ks, DistTree(vs, prng))
      }


    // -- Dist API --

    /** Scale the weights of this distribution by a given value. */
    def scale(w: Prob): Dist[A] = {
      new DistMap(_dist map { case (x, p) => (x, p * w) })
    }

    /**
     * Compress this distribution by accumulating weights of identical
     * support values.
     *
     * NOTE: This is a no-op.  [[DistMap]] are consolidated by
     * construction.
     */
    @inline def consolidate: Dist[A] = this

    /**
     * Normalize this distribution.
     *
     * Note: If the total weight of the distribution is zero, the
     * function returns the empty distribution.
     */
    def normalize: Dist[A] =
      if (_dist.isEmpty) this
      else if (totalWeight == 0.0) DistMap()
      else this.scale(1 / totalWeight)

    @inline def totalWeight = if (_dist.isEmpty) 0.0 else _distTree.totalWeight
    @inline def nextSample = if (_dist.isEmpty) {
      throw new NoSuchElementException("attempt to sample from empty distribution")
    } else _supportSeq(_distTree.nextRandom._1)

    @inline def orElse[B >: A](that: Dist[B]): Dist[B] = this ++ that
    @inline def +[B >: A](vp: (B, Prob)): Dist[B] = {
      val m = this.toMap[B, Prob]
      val vp1 = (vp._1, m.getOrElse(vp._1, 0.0) + vp._2)
      new DistMap(m + vp1)
    }

    // -- Iterable[A, Double] API --

    def ++[B >: A](xs: Map[B, Prob]): Dist[B] = {
      val m = toMap[B, Prob]

      // Merge the smaller map into the bigger one.
      val (z, ys) = if (xs.size > m.size) (xs, m) else (m, xs)
      val d = (z /: ys) { (z, y) =>
        val vp = (y._1, z.getOrElse(y._1, 0.0) + y._2)
        z + vp
      }
      new DistMap(d)
    }

    @inline def ++[B >: A](xs: GenTraversableOnce[(B, Prob)]): Dist[B] = {
      val m: Map[B, Prob] = xs.toMap[B, Prob] match {
        case m: Map[B, Prob] => m
        case m => Map() ++ m  // Convert generic maps to immutable ones.
      }
      this ++ m
    }

    @inline override def addString(b: scala.StringBuilder,
      start: String, sep: String, end: String): scala.StringBuilder =
      _dist.addString(b, start, sep, end)

    @inline override def filter(f: ((A, Prob)) => Boolean): Dist[A] =
      new DistMap(_dist.filter(f))

    @inline
    override def foreach[B](f: ((A, Prob)) => B): Unit = _dist.foreach(f)

    @inline override def iterator: Iterator[(A, Double)] = _dist.iterator

    @inline
    override protected[this] def newBuilder: Builder[(A, Prob), Dist[A]] =
      DistMap.newBuilder

    @inline override def stringPrefix = "DistMap"

    @inline
    override def toMap[T, U](implicit ev: (A, Prob) <:< (T, U)): Map[T, U] =
      _dist.asInstanceOf[Map[T, U]]


    // -- Any API --

    override def equals(that: Any) = that match {
      case that: DistMap[_] =>
        (this eq that) || (that canEqual this) && (this.toMap == that.toMap)
      case _ => false
    }
  }

  /** Companion object of the [[DistMap]] class. */
  object DistMap {

    import scala.collection.mutable.MapBuilder
    import scala.collection.generic.CanBuildFrom

    @inline def apply[A](m: Map[A, Prob]) = new Dist(m)
    @inline def apply[A](xs: (A, Prob)*) = new Dist(xs.toMap)

    class DistMapBuilder[A] extends Builder[(A, Prob), Dist[A]] {
      protected var elems = Map.empty[A, Prob]
      def +=(x: (A, Prob)): this.type = {
        val vp = (x._1, elems.getOrElse(x._1, 0.0) + x._2)
        elems = elems + vp
        this
      }
      def clear() { elems = Map.empty[A, Prob] }
      def result: Dist[A] = DistMap(elems)
    }

    def newBuilder[A]: Builder[(A, Prob), Dist[A]] = new DistMapBuilder[A]
    implicit def canBuildFrom[A]: CanBuildFrom[Dist[_], (A, Prob), Dist[A]] =
      new CanBuildFrom[Dist[_], (A, Prob), Dist[A]] {
        def apply(from: Dist[_]): Builder[(A, Prob), Dist[A]] = newBuilder[A]
        def apply(): Builder[(A, Prob), Dist[A]] = newBuilder
      }
  }

  @inline def dist[A](xs: (A, Prob)*): Dist[A] = DistMap(xs: _*)

  @inline def scale[A](w: Prob, xs: Dist[A]): Dist[A] = xs.scale(w)

  @inline def consolidate[A](xs: Dist[A]): Dist[A] = xs.consolidate

  @inline def normalize[A](xs: Dist[A]): Dist[A] = xs.normalize
}

package odds

import language.experimental.macros
import language.implicitConversions


/** Lifted logic and arithmetic operations. */
trait OddsLang extends OddsIntf with EmbeddedControls {

  import macros.MonadLifter
  import macros.RandMacroOps
  import probMonad._

  /** Refined abstract random variable type. */
  type Rand[+A] <: RandIntf[A]

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
     * @param cond the observation (random variable) to condition on.
     * @return the conditional probability of `this` given `cond`,
     *         i.e. `P(this | that)`
     */
    def when(cond: Rand[Boolean]): Rand[A] = bind {
      c: Boolean => if(c) this else never
    } (cond)
  }


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
    probMonad.choose(Dist(xs: _*))

  /**
   * Make a probabilistic choice.
   *
   * @tparam A the type of the support of the random variable
   *         returned by this function.
   * @param xs A distribution to chose from.
   * @return a random variable representing the outcome of the choice.
   */
  @inline final def choose[A](xs: Dist[A]): Rand[A] = probMonad.choose(xs)

  /** Make a (discrete) probabilistic choice. */
  @deprecated("use `choose` instead", "2013-08-19")
  @inline final def choice[A, M[A]](xs: (A, Prob)*) = choose(xs: _*)

  @inline final def always[A](x: A) = unit(x)

  @inline final def never[A] = zero

  def flip(p: Double): Rand[Boolean] = choose(true -> p, false -> (1-p))

  def uniform[A](xs: A*): Rand[A] = if (xs.isEmpty) never else {
    val p = 1.0 / xs.size
    choice(xs.map((_, p)):_*)
  }

  import probMonad.ToScalaMonadic

  def liftOp2[A, B, C](x: Rand[A], y: Rand[B])(f: (A, B) => C): Rand[C] =
    for (a <- x; b <- y) yield f(a, b)

  // -- Lifted tuple constructors --
  implicit def make_tuple2[T1, T2](t: (Rand[T1], Rand[T2])): Rand[(T1, T2)] =
    liftOp2(t._1, t._2)((_, _))
  def tuple2_get1[T](t: Rand[(T, _)]) : Rand[T] = t.map(_._1)
  def tuple2_get2[T](t: Rand[(_, T)]) : Rand[T] = t.map(_._2)

  implicit def make_tuple3[T1, T2, T3](
    t: (Rand[T1], Rand[T2], Rand[T3])): Rand[(T1, T2, T3)] =
    for (t1 <- t._1; t2 <- t._2; t3 <- t._3) yield (t1, t2, t3)
  def tuple3_get1[T](t: Rand[(T, _, _)]) : Rand[T] = t.map(_._1)
  def tuple3_get2[T](t: Rand[(_, T, _)]) : Rand[T] = t.map(_._2)
  def tuple3_get3[T](t: Rand[(_, _, T)]) : Rand[T] = t.map(_._3)

  // -- Lifted logic operations/relations --
  def infix_&&(x: Rand[Boolean], y: => Rand[Boolean]): Rand[Boolean] =
    x flatMap (if (_) y else always(false))
  def infix_||(x: Rand[Boolean], y: => Rand[Boolean]): Rand[Boolean] =
    x flatMap (if (_) always(true) else y)
  //def __equals[T](x: Rand[T], y: Rand[T]): Rand[Boolean] = // FIXME!
  //  liftOp2(x,y)(_ == _)
  def infix_===[T](x: Rand[T], y: Rand[T]): Rand[Boolean] =
    liftOp2(x, y)(_ == _)
  def infix_!=[T](x: Rand[T], y: Rand[T]): Rand[Boolean] =
    liftOp2(x, y)(_ != _)

  def __ifThenElse[T](cond: Rand[Boolean], tb: => Rand[T], fb: => Rand[T]) =
    cond flatMap { case true => tb; case false => fb }

  // HACK -- bug in scala-virtualized
  override def __ifThenElse[T](cond: =>Boolean, thenp: => T, elsep: => T) = cond match {
    case true => thenp
    case false => elsep
  }
}


/** Functions for pretty printing. */
trait OddsPrettyPrint {
  this: DistIntf =>

  def pp[A](dist: Dist[A], normalize: Boolean) = {
    val d = if (normalize) this.normalize(dist) else dist
    val dSorted = d.toList.sortBy{ case (x, p) => (-p, x.toString) }
    (dSorted map {
      case (x, p) => x + " : " + p
    }).mkString("\n")
  }

  def show[A](dist: Dist[A], desc: String = "", normalize: Boolean = false) = {
    println(desc)
    println(pp(dist, normalize))
    println("")
  }
}

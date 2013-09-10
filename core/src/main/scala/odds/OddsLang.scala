package ch.epfl.lamp.odds

import language.implicitConversions

/** Odds core language API. */
trait OddsLang extends OddsIntf with EmbeddedControls {

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
  @inline def flip(rp: Rand[Double]): Rand[Rand[Boolean]] = fmap(flip _)(rp)

  def uniform[A](xs: A*): Rand[A] = if (xs.isEmpty) never else {
    val p = 1.0 / xs.length
    choose(xs.map((_, p)): _*)
  }
  @inline def uniform[A](rxs: Rand[Seq[A]]): Rand[Rand[A]] =
    fmap((xs: Seq[A]) => uniform(xs: _*))(rxs)

  /** Condition a probabilistic choice on a Boolean random variable. */
  @inline def cond[A](rx: => Rand[A], rc: Rand[Boolean]): Rand[A] = bind {
    c: Boolean => if (c) rx else zero
  } (rc)


  // -- Lifted function applications --
  implicit def liftFun1Fmap[A, B](f: A => B): Rand[A] => Rand[B] =
    fmap(f) _
  implicit def liftFun1Bind[A, B](f: A => Rand[B]): Rand[A] => Rand[B] =
    bind(f) _


  // -- Lifted tuple/case class construction --
  type RandProduct[P <: Product] = macros.MonadicProduct[Rand, P]
  implicit def prodToRandProd[P <: Product](p: P)(
    implicit rp: RandProduct[P]) = rp.lift(p)


  // -- Lifted logic operations/relations --
  def infix_&&(x: Rand[Boolean], y: => Rand[Boolean]): Rand[Boolean] =
    bind { c: Boolean => if (c) y else always(false) } (x)
  def infix_||(x: Rand[Boolean], y: => Rand[Boolean]): Rand[Boolean] =
    bind { c: Boolean => if (c) always(true) else y } (x)

  // FIXME: Replace this with YinYang-type VirtualAny/VirtualAnyRef
  def infix_equals[A, B](x:  A,       ry: Rand[B]): Rand[Boolean] =
    unit(x).__equals(ry)
  def infix_equals[A, B](rx: Rand[A], y:  B      ): Rand[Boolean] =
    rx.__equals(unit(y))
  def infix_equals[A, B](rx: Rand[A], ry: Rand[B]): Rand[Boolean] =
    rx.__equals(ry)
  def __equal[A, B](x:  A,       ry: Rand[B]): Rand[Boolean] = unit(x).__==(ry)
  def __equal[A, B](rx: Rand[A], y:  B      ): Rand[Boolean] = rx.__==(unit(y))
  def __equal[A, B](rx: Rand[A], ry: Rand[B]): Rand[Boolean] = rx.__==(ry)
  def infix_!=[A, B](x:  A,       ry: Rand[B]): Rand[Boolean] = unit(x).__!=(ry)
  def infix_!=[A, B](rx: Rand[A], y:  B      ): Rand[Boolean] = rx.__!=(unit(y))
  def infix_!=[A, B](rx: Rand[A], ry: Rand[B]): Rand[Boolean] = rx.__!=(ry)

  def __ifThenElse[T](
    cond: Rand[Boolean], tb: => Rand[T], fb: => Rand[T]): Rand[T] =
    bind { c: Boolean => if (c) tb else fb } (cond)
  // HACK -- bug in scala-virtualized
  override def __ifThenElse[T](cond: => Boolean, tb: => T, fb: => T): T =
    cond match { case true => tb; case false => fb }


  /**
   * Implicit conversion from lists of random variables to random
   * lists.
   *
   * @todo Can/should we make this more generic?  E.g. implicit
   *   conversions form `Traversable[Rand[A]]` to
   *   `Rand[Traversable[A]]`?
   */
  implicit def listRandToRandList[A](l: List[Rand[A]]): Rand[List[A]] =
    l match {
      case rx :: rxs => bind { x: A =>
        fmap { xs: List[A] => x :: xs } (listRandToRandList(rxs))
      } (rx)
      case Nil => unit(Nil)
    }
}

package odds

import language.implicitConversions

/** Lifted logic and arithmetic operations. */
trait OddsLang extends OddsIntf with EmbeddedControls {

  import probMonad._

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

  @inline implicit final def always[A](x: A): Rand[A] = unit(x)

  @inline final def never[A]: Rand[A] = zero

  @inline implicit final def flatten[A](mmx: Rand[Rand[A]]): Rand[A] =
    join(mmx)

  @inline def flip(p: Double): Rand[Boolean] =
    choose(true -> p, false -> (1-p))

  def uniform[A](xs: A*): Rand[A] = if (xs.isEmpty) never else {
    val p = 1.0 / xs.size
    choice(xs.map((_, p)):_*)
  }

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
  def __equal[T](x: Rand[T], y: Rand[T]): Rand[Boolean] = x.__equals(y)
  def infix_===[T](x: Rand[T], y: Rand[T]): Rand[Boolean] = x.__==(y)
  def infix_!=[T](x: Rand[T], y: Rand[T]): Rand[Boolean] = x.__!=(y)

  def __ifThenElse[T](cond: Rand[Boolean], tb: => Rand[T], fb: => Rand[T]) =
    cond flatMap { case true => tb; case false => fb }


  // HACK -- bug in scala-virtualized
  override def __ifThenElse[T](cond: =>Boolean, thenp: => T, elsep: => T) = cond match {
    case true => thenp
    case false => elsep
  }

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

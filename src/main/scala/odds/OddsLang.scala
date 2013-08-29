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

  /**
   * Implicit conversion from lists of random variables to random
   * lists.
   *
   * @todo: Can/should we make this more generic?  E.g. implicit
   * conversions form `Traversable[Rand[A]]` to `Rand[Traversable[A]]`?
   */
  implicit def listRandToRandList[A](l: List[Rand[A]]): Rand[List[A]] =
    l match {
      case x :: xs => liftOp2(x, listRandToRandList(xs))(_ :: _)
      case Nil => always(Nil)
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

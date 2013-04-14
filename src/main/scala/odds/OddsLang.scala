package odds

import language.implicitConversions


/** Lifted logic and arithmetic operations. */
trait OddsLang extends EmbeddedControls with OddsIntf with DistIntf {

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
  def infix_&&(x: Rand[Boolean], y: Rand[Boolean]): Rand[Boolean] =
    x flatMap { c => if (c) y else always(false) }
  def infix_||(x: Rand[Boolean], y: Rand[Boolean]): Rand[Boolean] =
    x flatMap { c => if (c) always(true) else y }
  //def __equals[T](x: Rand[T], y: Rand[T]): Rand[Boolean] = // FIXME!
  //  liftOp2(x,y)(_ == _)
  def infix_===[T](x: Rand[T], y: Rand[T]): Rand[Boolean] =
    liftOp2(x,y)(_ == _)

  // -- Lifted arithmetic operations/relations --
  def infix_+(x: Rand[Int], y: Rand[Int]): Rand[Int] = liftOp2(x,y)(_ + _)
  def infix_-(x: Rand[Int], y: Rand[Int]): Rand[Int] = liftOp2(x,y)(_ - _)

  // -- Virtualized control -- // FIXME!
  //def __ifThenElse[T](cond: Rand[Boolean], tb: => Rand[T], fb: => Rand[T]) =
  //  cond flatMap { case true => tb; case false => fb }
}


/** Functions for pretty printing. */
trait OddsPrettyPrint {
  this: DistIntf =>

  def pp[A](dist: Dist[A]) = {
    val d = normalize(dist)
    val dSorted = d.toList.sortBy{ case (x, p) => (-p, x.toString) }
    (dSorted map {
      case (x, p) => x + " : " + p
    }).mkString("\n")
  }

  def show[A](dist: Dist[A], desc: String = "") = {
    println(desc)
    println(pp(dist))
    println("")
  }
}

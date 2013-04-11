package odds

/** Lifted logic and arithmetic operations. */
trait OddsLang extends EmbeddedControls with OddsIntf with DistIntf {

  def liftOp2[A, B, C](x: Rand[A], y: Rand[B])(f: (A, B) => C): Rand[C] =
    for (a <- x; b <- y) yield f(a, b)

  // -- Lifted logic operations/relations --
  def infix_&&(x: Rand[Boolean], y: Rand[Boolean]): Rand[Boolean] =
    x flatMap { c => if (c) y else always(false) }
  def infix_||(x: Rand[Boolean], y: Rand[Boolean]): Rand[Boolean] =
    x flatMap { c => if (c) always(true) else y }
  def infix_===[A](x: Rand[A], y: Rand[A]): Rand[Boolean] =
    liftOp2(x,y)(_ == _)

  // -- Lifted arithmetic operations/relations --
  def infix_+(x: Rand[Int], y: Rand[Int]): Rand[Int] = liftOp2(x,y)(_ + _)
  def infix_-(x: Rand[Int], y: Rand[Int]): Rand[Int] = liftOp2(x,y)(_ - _)
}

/** Functions for pretty printing. */
trait OddsPrettyPrint {
  this: OddsIntf =>

  def pp[A](r: Rand[A], strategy: String, solutions: Int) = {
    //val d = if (solutions > 0) r.reify(strategy, solutions) else r.reify
    val d = r.reify
    val dSorted = d.toList.sortBy{ case (x, p) => (-p, x.toString) }
    (dSorted map {
      case (x, p) => x + " : " + p
    }).mkString("\n")
  }

  def show[A](r: Rand[A], desc: String = "", strategy: String = "", solutions: Int = -1) = {
    println(desc)
    println(pp(r,strategy,solutions))
    println("")
  }
}

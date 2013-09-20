package ch.epfl.lamp.odds
package macros

/**
 * Macros for generic operations on random variables.
 *
 * This trait provides macros for some generic operation on random
 * variables that can not easily be provided through implicit views or
 * other "standard" overloading techniques.
 *
 * @tparam R the random variable type.
 */
trait RandMacroOps[R[+_]] {

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
   *         i.e. `P(this | cond)`
   */
  def when(cond: R[Any]): R[Any] = macro RandMacroOps.when[R[Any]]

  /**
   * Macro for generic `+` operation on random variables.
   *
   * This macro is necessary because any value (and hence any random
   * variable) can be converted into a [[scala.runtime.StringAdd]]
   * through [[scala.Predef.any2stringadd]].  The `+` operation in
   * [[scala.runtime.StringAdd]] takes precedence over automatic
   * lifting through [[MonadLifter]].
   *
   * An alternative workaround would be to add an implicit conversion
   * for all numeric types (i.e. those that are instances of the
   * [[scala.math.Numeric]] type class), but this requires extra
   * support for weak type coercion between different numeric types
   * (e.g. to be able to add floating point values and integers).
   *
   * @param that the value that `this` should be added to.
   */
  def +(that: Any): R[Any] = macro RandMacroOps.+[R[Any]]

  /** Compare two random variables for equality. */
  def __equals(that: Any): R[Any] = macro RandMacroOps.__equals[R[Any]]

  /** Compare two random variables for equality. */
  def __==(that: Any): R[Any] = macro RandMacroOps.__==[R[Any]]

  /** Compare two random variables for inequality. */
  def __!=(that: Any): R[Any] = macro RandMacroOps.__!=[R[Any]]

  /** Convert a random variable into a (random) string. */
  def __toString: R[Any] = macro RandMacroOps.__toString[R[Any]]
}

/** Companion object of the [[RandMacroOps]] trait. */
private object RandMacroOps {

  import reflect.macros.Context
  import MonadLifter.liftOp
  import VirtualAny.liftAnyOp
  import VirtualControls.liftIfThenElse

  /** Condition a random variable on an observation. */
  def when[R](c: Context)(cond: c.Expr[Any])(
    implicit rt: c.WeakTypeTag[R]): c.Expr[R] = {

    import c.universe._
    val lb = new MonadLifter.LifterBundle[c.type](c) { val monad = rt.tpe }
    val zeroTree = lb.typeCheck(q"${lb.monadClassInst}.zero")
    val zeroExpr = c.Expr[R](zeroTree)
    val thisExpr = c.Expr[R](c.prefix.tree)
    VirtualControls.liftIfThenElse(c)(rt.tpe)(cond, thisExpr, zeroExpr)
  }

  /** Generic '+' operation on random variables. */
  def +[R: c.WeakTypeTag](c: Context)(that: c.Expr[Any]): c.Expr[R] = {
    liftOp(c)(implicitly[c.WeakTypeTag[R]].tpe)("$plus", List(c.prefix, that))
  }

  /** Compare two random variables for equality. */
  def __equals[R: c.WeakTypeTag](c: Context)(that: c.Expr[Any]): c.Expr[R] = {
    liftAnyOp(c)(implicitly[c.WeakTypeTag[R]].tpe)("equals", List(c.prefix, that))
  }

  /** Compare two random variables for equality. */
  def __==[R: c.WeakTypeTag](c: Context)(that: c.Expr[Any]): c.Expr[R] = {
    liftAnyOp(c)(implicitly[c.WeakTypeTag[R]].tpe)("==", List(c.prefix, that))
  }

  /** Compare two random variables for inequality. */
  def __!=[R: c.WeakTypeTag](c: Context)(that: c.Expr[Any]): c.Expr[R] = {
    liftAnyOp(c)(implicitly[c.WeakTypeTag[R]].tpe)("!=", List(c.prefix, that))
  }

  /** Convert a random variable into a (random) string. */
  def __toString[R: c.WeakTypeTag](c: Context): c.Expr[R] = {
    liftAnyOp(c)(implicitly[c.WeakTypeTag[R]].tpe)("toString", List(c.prefix))
  }
}

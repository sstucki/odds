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
  def +(that: Any): R[Any] = macro RandMacroOps.+[R]

  /** Compare two random variables for equality. */
  def __equals(that: Any): R[Any] = macro RandMacroOps.__equals[R]

  /** Compare two random variables for equality. */
  def __==(that: Any): R[Any] = macro RandMacroOps.__==[R]

  /** Compare two random variables for inequality. */
  def __!=(that: Any): R[Any] = macro RandMacroOps.__!=[R]

  /** Convert a random variable into a (random) string. */
  def __toString: R[Any] = macro RandMacroOps.__toString[R]
}

/** Companion object of the [[RandMacroOps]] trait. */
private object RandMacroOps {

  import reflect.macros.Context

  /** Lift an operation into `R[_]`. */
  def liftOp[R[+_]](c: Context)(
    rop: String, op: String, rtype: c.Type, args: c.Expr[Any]*)(
    implicit rt: c.WeakTypeTag[R[_]]): c.Expr[R[Any]] = {

    import c.universe._
    import MonadLifter._

    val lb = new LifterBundle[c.type](c) { val monad = rt.tpe }
    val args1 = args.toList.map(a => (a.tree, a.actualType))
    val innerOp = if (lb.monadDepth(rtype) <= 1) op else rop
    c.Expr(lb.lift(newTermName(innerOp), args1, Nil, rtype))
  }

  /** Generic '+' operation on random variables. */
  def +[R[+_]](c: Context)(that: c.Expr[Any])(
    implicit rt: c.WeakTypeTag[R[_]]): c.Expr[R[Any]] = {
    liftOp(c)("$plus", "$plus", c.prefix.actualType, c.prefix, that)
  }

  /** Compare two random variables for equality. */
  def __equals[R[+_]](c: Context)(that: c.Expr[Any])(
    implicit rt: c.WeakTypeTag[R[_]]): c.Expr[R[Any]] = {
    liftOp(c)("__equals", "equals", c.prefix.actualType, c.prefix, that)
  }

  /** Compare two random variables for equality. */
  def __==[R[+_]](c: Context)(that: c.Expr[Any])(
    implicit rt: c.WeakTypeTag[R[_]]): c.Expr[R[Any]] = {
    liftOp(c)("__$eq$eq", "$eq$eq", c.prefix.actualType, c.prefix, that)
  }

  /** Compare two random variables for inequality. */
  def __!=[R[+_]](c: Context)(that: c.Expr[Any])(
    implicit rt: c.WeakTypeTag[R[_]]): c.Expr[R[Any]] = {
    liftOp(c)("__$bang$eq", "$bang$eq", c.prefix.actualType, c.prefix, that)
  }

  /** Convert a random variable into a (random) string. */
  def __toString[R[+_]](c: Context)(
    implicit rt: c.WeakTypeTag[R[_]]): c.Expr[R[Any]] = {
    liftOp(c)("__toString", "toString", c.prefix.actualType, c.prefix)
  }
}

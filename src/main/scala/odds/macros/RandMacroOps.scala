package odds
package macros

import language.experimental.macros

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
}

private object RandMacroOps {

  import reflect.macros.Context
  import MonadLifter._

  /** Lift the addition of the prefix and `that` into `R[_]`. */
  def +[R[+_]](c: Context)(that: c.Expr[Any])(
    implicit rt: c.WeakTypeTag[R[_]]): c.Expr[R[Any]] = {
    import c.universe._
    val lb = new LifterBundle[c.type](c) { val monad = rt.tpe }
    val args = List(c.prefix.tree, that.tree).map(a => (a, a.tpe))
    c.Expr(lb.lift(newTermName("$plus"), args, Nil, weakTypeOf[R[Any]]))
  }
}

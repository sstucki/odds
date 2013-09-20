package ch.epfl.lamp.odds
package macros

import internal.OddsBase

/**
 * Member method-based virtualization of the `Any` API.
 *
 * This trait provides macro definitions for the infix methods
 * (as provided by Scala virtualize) corresponding to the `Any` API.
 * The macros lift virtualized method calls into the probability monad
 * `Rand[_]`.
 *
 * Example: When faced with an expression of the form `x != y`, the
 * Scala Virtualized compiler (or the `@virtualized` macro annotation)
 * will generate a method call: `infix_!=(x, y)`.  This method call
 * will be bound to an implementation based on normal rules of
 * scoping.  If it binds to the one in this trait, the corresponding
 * macro will rewrite the call to e.g.
 * {{{
 *   Rand.bind(y) { y0: B =>
 *     Rand.bind(x) { x0: A =>
 *       x0.!=(y0)
 *     }
 *   }
 * }}}
 * if `x` and `y` are of type `Rand[A]` and `Rand[B]`, respectively.
 */
trait VirtualAny extends EmbeddedControls { this: OddsBase =>

  import VirtualAny.MacrosInOddsBase._

  // Poor man's infix methods for `Any` methods
  def __equal(x1: Any, x2: Rand[Any]): Any = macro any_==
  def __equal(x1: Rand[Any], x2: Any): Any = macro any_==
  def __equal(x1: Rand[Any], x2: Rand[Any]): Any = macro any_==
  def infix_==(x1: Any, x2: Rand[Any]): Any = macro any_==
  def infix_==(x1: Rand[Any], x2: Any): Any = macro any_==
  def infix_==(x1: Rand[Any], x2: Rand[Any]): Any = macro any_==
  def infix_!=(x1: Any, x2: Rand[Any]): Any = macro any_!=
  def infix_!=(x1: Rand[Any], x2: Any): Any = macro any_!=
  def infix_!=(x1: Rand[Any], x2: Rand[Any]): Any = macro any_!=
  def infix_##(x: Rand[Any]): Any = macro any_##
  def infix_equals(x1: Any, x2: Rand[Any]): Any = macro any_equals
  def infix_equals(x1: Rand[Any], x2: Any): Any = macro any_equals
  def infix_equals(x1: Rand[Any], x2: Rand[Any]): Any = macro any_equals
  def infix_hashCode(x: Rand[Any]): Any = macro any_hashCode
  def infix_asInstanceOf[T](x: Rand[Any]): Any = macro any_asInstanceOf[T]
  def infix_isInstanceOf[T](x: Rand[Any]): Any = macro any_isInstanceOf[T]
  def infix_toString(x: Rand[Any]): Any = macro any_toString
  def infix_getClass(x: Rand[Any]): Any = macro any_getClass
}

/**
 * Companion object of [[VirtualAny]].
 *
 * This object provides macro implementations for the infix methods
 * (as provided by Scala virtualize) corresponding to the `Any` API.
 * The macros lift virtualized method calls into the probability monad
 * `Rand[_]`.
 */
object VirtualAny {

  import reflect.macros.Context
  import MonadLifter.liftOp

  /**
   * Lift an operation from the virtualized `Any` API into the monadic
   * domain.
   *
   * @tparam R the result type of the method.
   * @param c the context of the macro call.
   * @param monadType the monad type to lift the operation into.
   * @param name the name of the operation.
   * @param args the (term) arguments of the operation.
   * @param targs the type arguments of the operation.
   */
  @inline def liftAnyOp[R](c: Context)(monadType: c.Type)(
    name: String, args: List[c.Expr[Any]],
    targs: List[c.Type] = Nil): c.Expr[R] = {
    liftOp(c)(monadType)(name, args, targs, Some("__" + name))
  }

  // The macro implementations in this object may only be called from
  // the companion trait (or subclasses thereof), to make sure the
  // prefix has type `internal.OddsBase`.
  private object MacrosInOddsBase {

    def liftAnyOp[R](c: Context)(
      name: String, args: List[c.Expr[Any]],
      targs: List[c.Type] = Nil): c.Expr[R] = {

      import c.universe._

      // Get the `Rand[_]` type from the prefix (which we know to be
      // of type `internal.OddsBase`).
      val oddsIntfType = c.prefix.actualType
      val randTypeSym = oddsIntfType.member(newTypeName("Rand")).asType
      val randType = typeRef(oddsIntfType, randTypeSym, List(typeOf[Any]))

      // Do the actual lifting.
      VirtualAny.liftAnyOp(c)(randType)(name, args, targs)
    }

    def any_==(c: Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Any] = {
      liftAnyOp(c)("==", List(x1, x2))
    }

    def any_!=(c: Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Any] = {
      liftAnyOp(c)("!=", List(x1, x2))
    }

    def any_##(c: Context)(x: c.Expr[Any]): c.Expr[Any] = {
      liftAnyOp(c)("##", List(x))
    }

    def any_equals(c: Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Any] = {
      liftAnyOp(c)("equals", List(x1, x2))
    }

    def any_hashCode(c: Context)(x: c.Expr[Any]): c.Expr[Any] = {
      liftAnyOp(c)("hashCode", List(x))
    }

    def any_asInstanceOf[T](c: Context)(x: c.Expr[Any])(
      implicit tt: c.WeakTypeTag[T]): c.Expr[Any] = {
      liftAnyOp(c)("asInstanceOf", List(x), List(tt.tpe))
    }

    def any_isInstanceOf[T](c: Context)(x: c.Expr[Any])(
      implicit tt: c.WeakTypeTag[T]): c.Expr[Any] = {
      liftAnyOp(c)("isInstanceOf", List(x), List(tt.tpe))
    }

    def any_toString(c: Context)(x: c.Expr[Any]): c.Expr[Any] = {
      liftAnyOp(c)("toString", List(x))
    }

    def any_getClass(c: Context)(x: c.Expr[Any]): c.Expr[Any] = {
      liftAnyOp(c)("getClass", List(x))
    }
  }
}

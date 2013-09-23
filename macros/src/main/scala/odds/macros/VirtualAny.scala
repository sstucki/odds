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

  // FIXME: sstucki: Unfortunately, the fully generic, macro-based
  // implementations of the infix functions don't seem to work with
  // current Scala Virtualized (as of 2013-09-23).  It's not quite
  // clear to me whether the bug is in Scala Virtualized or macros or
  // a combination of the two.  Possible solutions:
  //
  //  * Isolate and fix the bug in Scala Virtualized and/or macros.
  //    Might be non-trivial as this requires an investigation into
  //    how macros and Scala Virtualized are supposed to interact.
  //
  //  * Don't use macro implementations.  This will require advanced
  //    type-level programming if we don't want to abandon deep
  //    lifting (i.e. lifting across nested monadic types).
  //
  //  * Don't use Scala Virtualized.  This requires some other form of
  //    language virtualization, likely to be macro-based
  //    e.g. an @virtualize or @odds macro annotation that perform the
  //    virtualization transformation.

  // def __equal(x1: Any, x2: Any): Any = macro any_==
  // def infix_==(x1: Any, x2: Any): Any = macro any_==
  // def infix_!=(x1: Any, x2: Any): Any = macro any_!=
  // def infix_##(x: Any): Any = macro any_##
  // def infix_equals(x1: Any, x2: Any): Any = macro any_equals
  // def infix_hashCode(x: Any): Any = macro any_hashCode
  // def infix_asInstanceOf[T](x: Any): Any = macro any_asInstanceOf[T]
  // def infix_isInstanceOf[T](x: Any): Any = macro any_isInstanceOf[T]
  // def infix_toString(x: Any): Any = macro any_toString
  // def infix_getClass(x: Any): Any = macro any_getClass

  // FIXME (cont.): These are method-based implementation of the infix
  // methods corresponding to the `Any` API.  Proper lifting relies on
  // overloading resolution on parameter types: any combination of
  // plain and monadic argument types needs to be implemented to
  // ensure the appropriate lifting is used.  Unfortunately, this
  // technique does not handle deep lifting correctly.
  import Rand._
  def __equal(x1: Any, rx2: Rand[Any]): Rand[Boolean] = fmap(rx2)(x1 ==  _)
  def __equal(rx1: Rand[Any], x2: Any): Rand[Boolean] = fmap(rx1)(_ == x2)
  def __equal(rx1: Rand[Any], rx2: Rand[Any]): Rand[Boolean] = bind(rx2)(rx1 == _)
  def infix_==(x1: Any, rx2: Rand[Any]): Rand[Boolean] = fmap(rx2)(x1 == _)
  def infix_==(rx1: Rand[Any], x2: Any): Rand[Boolean] = fmap(rx1)(_ == x2)
  def infix_==(rx1: Rand[Any], rx2: Rand[Any]): Rand[Boolean] = bind(rx2)(rx1 == _)
  def infix_!=(x1: Any, rx2: Rand[Any]): Rand[Boolean] = fmap(rx2)(x1 != _)
  def infix_!=(rx1: Rand[Any], x2: Any): Rand[Boolean] = fmap(rx1)(_ != x2)
  def infix_!=(rx1: Rand[Any], rx2: Rand[Any]): Rand[Boolean] = bind(rx2)(rx1 != _)
  def infix_##(rx: Rand[Any]): Rand[Int] = fmap(rx)(_.##)
  def infix_equals(x1: Any, rx2: Rand[Any]): Rand[Boolean] = fmap(rx2)(x1 equals _)
  def infix_equals(rx1: Rand[Any], x2: Any): Rand[Boolean] = fmap(rx1)(_ equals x2)
  def infix_equals(rx1: Rand[Any], rx2: Rand[Any]): Rand[Boolean] = bind(rx2)(rx1 equals _)
  def infix_hashCode(rx: Rand[Any]): Rand[Int] = fmap(rx)(_.hashCode)
  def infix_asInstanceOf[T](rx: Rand[Any]): Rand[T] = fmap(rx)(_.asInstanceOf[T])
  def infix_isInstanceOf[T](rx: Rand[Any]): Rand[Boolean] = fmap(rx)(_.isInstanceOf[T])
  def infix_toString(rx: Rand[Any]): Rand[String] = fmap(rx)(_.toString)
  def infix_getClass(rx: Rand[Any]): Rand[java.lang.Class[_]] = fmap(rx)(_.getClass)
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

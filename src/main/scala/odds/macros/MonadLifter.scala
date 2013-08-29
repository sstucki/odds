package odds
package macros

import language.experimental.macros

import reflect.macros.Context

import functors.MonadPlus

/**
 * Auto-lifter for [[MonadPlus]].
 *
 * This trait implements dynamic macros (aka "Poor man's type macros")
 * to lift methods defined in a class `A` into the monadic domain of a
 * given monad `M` (i.e. into `M[A]`).  It does so by intercepting
 * (using the `Dynamic` trait) and rewriting (using macros) any call
 * of the form `(r: M[A]).m(x, y, ..., z)` into
 * {{{
 *   bind { x0 =>
 *     bind { y0 =>
 *       ...
 *       bind { z0 =>
 *         fmap { r0 =>
 *           r0.m(x0, y0, ..., z0)
 *         }(r)
 *       }(z)
 *       ...
 *     }(y)
 *   }(x)
 * }}}
 * using the monadic operations `bind` and `fmap` provided by the
 * monad instance corresponding to `M`.
 *
 * FIXME: SI-7776 prevents implementation of type parameters.
 */
trait MonadLifter[M[+_]] extends DynamicMacro[M[_]] {

  import MonadLifter.Handler._

  override def applyDynamic(name: String)(args: Any*): M[_] =
    macro applyDynamicMacro[M[_]]
  // override def applyDynamic[T1](name: String)(args: Any*)(
  //   implicit d1: DummyImplicit): M[_] =
  //   macro applyDynamicMacro[T1, M[_]]
  // override def applyDynamic[T1, T2](name: String)(args: Any*): M[_] =
  //   macro applyDynamicMacro[T1, T2, M[_]]
  // override def applyDynamic[T1, T2, T3](name: String)(args: Any*): M[_] =
  //   macro applyDynamicMacro[T1, T2, T3, M[_]] */

  override def applyDynamicNamed(name: String)(
    args: (String, Any)*): M[_] =
    macro applyDynamicNamedMacro[M[_]]
  // override def applyDynamicNamed[T1](name: String)(
  //  args: (String, Any)*): M[_] =
  //   macro applyDynamicNamedMacro[T1, M[_]]
  // override def applyDynamicNamed[T1, T2](name: String)(
  //   args: (String, Any)*): M[_] =
  //   macro applyDynamicNamedMacro[T1, T2, M[_]]
  // override def applyDynamicNamed[T1, T2, T3](name: String)(
  //   args: (String, Any)*): M[_] =
  //   macro applyDynamicNamedMacro[T1, T2, T3, M[_]]

  override def selectDynamic(name: String): M[_] =
    macro selectDynamicMacro[M[_]]
  // override def selectDynamic[T1](name: String): M[_] =
  //   macro selectDynamicMacro[T1, M[_]]
  // override def selectDynamic[T1, T2](name: String): M[_] =
  //   macro selectDynamicMacro[T1, T2, M[_]]
  // override def selectDynamic[T1, T2, T3](name: String): M[_] =
  //   macro selectDynamicMacro[T1, T2, T3, M[_]]
}

/**
 * Companion object of [[MonadLifter]].
 *
 * This object contains the macro implementations of the
 * [[MonadLifter]] trait.
 */
object MonadLifter {

  /** Get the type argument of a monad type. */
  def getMonadTypeArg(c: Context)(
    monad: c.Type, monadInstance: c.Type): c.Type = {
    import c.universe._
    val rBase = monadInstance.baseType(monad.typeSymbol)
    val TypeRef(_, _, List(targ)) = rBase
    targ
  }

  /** Get the type class instance of `MonadPlus` for a given monad. */
  def getMonadClassInst(c: Context)(monad: c.Type): c.Tree = {
    import c.universe._
    class Dummy[+A]
    val monadPlusCons = weakTypeOf[MonadPlus[Dummy]].typeConstructor

    c.inferImplicitValue(
      appliedType(monadPlusCons, List(monad.typeConstructor)), false)
  }

  /** Lift a method/function call into the monadic domain. */
  def lift(c: Context)(call: c.Tree, monad: c.Type): c.Tree = {
    import c.universe._

    val (callee, args0, targs) = call match {
      case Apply(TypeApply(c, tas), as) => (c, as,  tas)
      case Apply(c, as)                 => (c, as,  Nil)
      case TypeApply(c, tas)            => (c, Nil, tas)
      case c                            => (c, Nil, Nil)
    }
    val (mtd, args) = callee match {
      case Select(q, n) => (Some(n), q :: args0)
      case _            => (None,    args0)
    }

    val monadAny = appliedType(monad.typeConstructor, List(typeOf[Any]))
    val (mtdArgs, bindArgss) = args.map { arg =>
      // Compute types for the closure arguments and the terms to pass
      // to the corresponding `bind`/`fmap` calls.  If the arguments
      // already of monad type `M[A]` extract `A`, otherwise don't
      // lift the corresponding argument but pass it directly to the
      // nested call.
      val atypeWidened = arg.tpe.widen
      if (atypeWidened <:< monadAny) {
        // Allocate a term name (and identifier) for the corresponding
        // argument of the closure passed to `bind`/`fmap`.
        val aname = newTermName(c.fresh("x$"))
        val aid = Ident(aname)

        // Extract the type concrete argument of the monad type.
        val atype = getMonadTypeArg(c)(monad, atypeWidened)
        (aid, List((atype, arg, aname)))
      } else {
        (arg, Nil)
      }
    }.unzip
    val bindArgs = bindArgss.flatten

    // Build the nested call.
    val (nCallee, nMtdArgs) = mtd match {
      case Some(n) => (Select(mtdArgs.head, n), mtdArgs.tail)
      case None    => (callee,                  mtdArgs)
    }
    val nCalleeAndTypes =
      if (targs.isEmpty) nCallee else TypeApply(nCallee, targs)
    val nCall =
      if (nMtdArgs.isEmpty) nCalleeAndTypes   // Selection
      else Apply(nCalleeAndTypes, nMtdArgs)   // Application

    val monadClassInst = getMonadClassInst(c)(monad)
    if (bindArgs.isEmpty) {                   // No arguments to lift
      q"$monadClassInst.unit{$nCall}"
    } else {                                  // Lift arguments
      val (atype1, aterm1, aname1) = bindArgs.head
      val fmap =
        q"$monadClassInst.fmap{($aname1: $atype1) => $nCall}($aterm1)"
      (bindArgs.tail :\ fmap) {
        case ((atype, aterm, aname), b) =>
          q"$monadClassInst.bind{($aname: $atype) => $b}($aterm)"
      }
    }
  }

  /** Dynamic macro handler. */
  private object Handler extends DynamicMacro.Handler {

    /** Handler for `applyDynamic` calls. */
    override def applyDynamic[M](c: Context)(name: String)(
      args: List[c.Tree])(targs: List[c.Type]): c.Expr[M] = {

      import c.universe._

      val Ident(mtd) = c.parse(name)
      val rterm = c.prefix.tree
      val ap =
        if (targs.tail.isEmpty) q"$rterm.$mtd(..$args)"
        else q"$rterm.$mtd[..${targs.tail}](..$args)"

      c.Expr(lift(c)(ap, targs.head))
    }

    // FIMXE: Implement handler for `applyDynamicNamed` calls.

    /** Handler for `selectDynamic` calls. */
    override def selectDynamic[M](c: Context)(name: String)(
      targs: List[c.Type]): c.Expr[M] = {

      import c.universe._

      val Ident(member) = c.parse(name)
      val rterm = c.prefix.tree
      val ap =
        if (targs.tail.isEmpty) q"$rterm.$member"
        else q"$rterm.$member[..${targs.tail}]"

      c.Expr(lift(c)(ap, targs.head))
    }
  }
}

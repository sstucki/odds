package ch.epfl.lamp.odds
package macros

import reflect.macros.Context
import reflect.macros.TypecheckException

import functors.MonadPlus


/**
 * Auto-lifter for [[functors.MonadPlus]].
 *
 * This trait implements dynamic macros (aka "Poor man's type macros")
 * to lift methods defined in a class `A` into the monadic domain of a
 * given monad `M` (i.e. into `M[A]`).  It does so by intercepting
 * (using the `Dynamic` trait) and rewriting (using macros) any call
 * of the form `(r: M[A]).m(x, y, ..., z)` into
 * {{{
 *   bind(z) { z0 =>
 *     ...
 *     bind(y) { y0 =>
 *       bind(x) { x0 =>
 *         fmap(r) { r0 =>
 *           r0.m(x0, y0, ..., z0)
 *         }
 *       }
 *     }
 *     ...
 *   }
 * }}}
 * using the monadic operations `bind` and `fmap` provided by the
 * monad instance corresponding to `M`.
 *
 * @todo FIXME: SI-7776 prevents implementation of type parameters.
 */
trait MonadLifter[M[+_]] extends DynamicMacro[M[Any]] {

  import MonadLifter.Handler._

  override def applyDynamic(name: String)(args: Any*): M[Any] =
    macro applyDynamicMacro[M[Any]]
  // override def applyDynamic[T1](name: String)(args: Any*)(
  //   implicit d1: DummyImplicit): M[_] =
  //   macro applyDynamicMacro[T1, M[_]]
  // override def applyDynamic[T1, T2](name: String)(args: Any*): M[_] =
  //   macro applyDynamicMacro[T1, T2, M[_]]
  // override def applyDynamic[T1, T2, T3](name: String)(args: Any*): M[_] =
  //   macro applyDynamicMacro[T1, T2, T3, M[_]] */

  override def applyDynamicNamed(name: String)(
    args: (String, Any)*): M[Any] =
    macro applyDynamicNamedMacro[M[Any]]
  // override def applyDynamicNamed[T1](name: String)(
  //  args: (String, Any)*): M[_] =
  //   macro applyDynamicNamedMacro[T1, M[_]]
  // override def applyDynamicNamed[T1, T2](name: String)(
  //   args: (String, Any)*): M[_] =
  //   macro applyDynamicNamedMacro[T1, T2, M[_]]
  // override def applyDynamicNamed[T1, T2, T3](name: String)(
  //   args: (String, Any)*): M[_] =
  //   macro applyDynamicNamedMacro[T1, T2, T3, M[_]]

  override def selectDynamic(name: String): M[Any] =
    macro selectDynamicMacro[M[Any]]
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

  /**
   * Convenience class for bundling macro implementations.
   *
   * @tparam C the type of the context `c`.
   * @param c The context of the macro call associated with this bundle.
   */
  abstract class LifterBundle[C <: Context](val c: C) {

    import c.universe._

    /** The monad type associated with this lifter bundle. */
    val monad: c.Type

    /** An instance of the monad type with underlying type `Any`. */
    lazy val monadAny = monadInst(typeOf[Any])

    /** Construct a monad instance. */
    def monadInst(inner: c.Type): c.Type = {
      appliedType(monad.typeConstructor, List(inner))
    }

    /** Get the type argument of a monad type. */
    def monadTypeArg(monadInstance: c.Type): c.Type = {
      val mBase = monadInstance.widen.baseType(monad.typeSymbol)
      mBase match {
        case TypeRef(_, _, List(targ)) => targ
        case _ => c.abort(c.enclosingPosition,
          "attempt to get type argument of non-type-reference type '" +
          mBase + "'")
      }
    }

    /** Check if a given type is an instance of `monad`. */
    def isMonadic(t: c.Type) = t match {
      case TypeRef(_, _, _ :: _) if t <:< monadAny => true
      case _                                       => false
    }

    /** Get the number of nested monadic layers of a monad type. */
    def monadDepth(monadInstance: c.Type): Int = {
      def loop(mInst: c.Type, depth: Int): Int = {
        val mWidened = mInst.widen
        if (isMonadic(mWidened)) loop(monadTypeArg(mWidened), depth + 1)
        else depth
      }
      loop(monadInstance, 0)
    }

    /** Get the type class instance of `MonadPlus` for a given monad. */
    def monadClassInst: c.Tree = {
      class Dummy[+A]
      val monadPlusCons = weakTypeOf[MonadPlus[Dummy]].typeConstructor
      c.inferImplicitValue(
        appliedType(monadPlusCons, List(monad.typeConstructor)), false)
    }

    /** Type check a tree. */
    def typeCheck(tree: c.Tree): c.Tree = {
      try c.typeCheck(tree) catch {
        case TypecheckException(_, msg) => c.abort(c.enclosingPosition,
          "type error during lifting: " + msg)
      }
    }

    /**
     * Lift a method call into the monadic domain.
     *
     * Unless the call to be lifted has at least one monadic argument,
     * the lifting will be trivial, i.e. the call will just be wrapped
     * into `unit(...)`.
     *
     * This method is trying to be smart about the overall return type
     * of the resulting lifted call.  If the expected monadic return
     * type of the resulting call is "deeper" (i.e. the number of
     * monad layers is higher) than the return type of the original
     * call, then the original call will be wrapped into an `fmap`
     * call, making the result type at least as deep as the deepest
     * argument to the resulting call.  On the other hand, if the
     * expected return type of the resulting call is at most as deep
     * as the result type of the original call, then the original call
     * will be wrapped into `bind` calls all the way, effectively
     * removing a monadic layer from the result type of the resulting
     * call.
     *
     * @param mtd the name of the callee.
     * @param retDepth the expected monadic depth of the return type of
     *   the resulting call.
     * @param args the arguments to the call and their types.  The first
     *   argument must be the receiver object of the method call.
     * @param targs the type arguments of the call.
     * @return the tree of the lifted call.
     */
    def lift(
      mtd: c.Name, retDepth: Int, args: List[(c.Tree, c.Type)] = Nil,
      targs: List[c.Type] = Nil): c.Tree = {

      def mkCall(mtd: c.Name, args: List[c.Tree], targs: List[c.Type]) = {
        val callee = Select(args.head, mtd)
        val mtdArgs = args.tail
        val calleeAndTypes =
          if (targs.isEmpty) callee
          else TypeApply(callee, targs.map(ta => TypeTree(ta)))
        if (mtdArgs.isEmpty) calleeAndTypes   // Selection
        else Apply(calleeAndTypes, mtdArgs)   // Application
      }

      val (mtdArgs, bindArgss) = args.map { case (arg, atype) =>
        // Compute the types for the closure arguments and the terms
        // to pass to the corresponding `bind`/`fmap` calls.  If the
        // arguments already are of monad type `M[A]` extract `A`,
        // otherwise don't lift the corresponding argument but pass it
        // directly to the nested call.
        val atypeWidened = atype.widen
        if (isMonadic(atypeWidened)) {
          // Allocate a term name (and identifier) for the
          // corresponding argument of the closure passed to
          // `bind`/`fmap`.
          val aname = newTermName(c.fresh("x$"))
          val aid = Ident(aname)

          // Extract the concrete type argument of the monad type.
          val nAtype = monadTypeArg(atypeWidened)
          ((aid, nAtype), List((nAtype, arg, aname)))
        } else {
          ((arg, atype), Nil)
        }
      }.unzip
      val bindArgs = bindArgss.flatten

      // Build the nested call.
      val (mtdArgNames, mtdArgTypes) = mtdArgs.unzip
      val nCall = mkCall(mtd, mtdArgNames, targs)

      val mClassInst = monadClassInst
      if (bindArgs.isEmpty) {                   // No arguments to lift
        if (retDepth == 0) nCall                // No lifting expected
        else q"$mClassInst.unit { $nCall }"     // Trivial lifting
      } else {                                  // Lift arguments

        // At this point, we need to infer the return type of `nCall`
        // in order to decide whether the innermost monadic option
        // should be a `bind` (if the return type is deep enough) or
        // an `fmap` (if the return type is shallower than the
        // expected overall return type).  Ideally, we'd just type
        // check `nCall` to infer its result type.  However, some of
        // the method arguments in `nCall` are free (and untyped)
        // identifiers, which might lead to the type checker not being
        // able to infer the return type correctly (e.g. because we
        // might be calling an overloaded method).  So instead, we
        // build a dummy function binding (and typing) the free
        // identifiers and performing `nCall` internally.  We can then
        // type check that dummy function and get the return type of
        // the internal `nCall`.
        val (dummyArgs, dummyArgNames) = mtdArgTypes.map { atype =>
          val aname = newTermName(c.fresh("x$"))
          val arg = ValDef(Modifiers(Flag.PARAM), aname, TypeTree(atype),
            EmptyTree)
          (arg, Ident(aname))
        }.unzip
        val dummyCall = mkCall(mtd, dummyArgNames, targs)
        val dummyFun = q"((..$dummyArgs) => $dummyCall)"
        val Function(_, typedCall) = typeCheck(dummyFun)
        val nRetType = typedCall.tpe
        val nRetDepth = monadDepth(nRetType)

        val (bindArgs1, nCall1) = if (nRetDepth >= retDepth) {
          // `nCall` returns deep result, use `bind`s all the way in.
          (bindArgs, nCall)
        } else {
          // `nCall` returns shallow result, wrap it into an `fmap`.
          val (atype1, aterm1, aname1) = bindArgs.head
          val fmap =
            q"$mClassInst.fmap($aterm1) { ($aname1: $atype1) => $nCall }"
          (bindArgs.tail, fmap)
        }

        // Add `bind`s for the remaining arguments.
        (bindArgs1 :\ nCall1) {
          case ((atype, aterm, aname), call) =>
            q"$mClassInst.bind($aterm) { ($aname: $atype) => $call }"
        }
      }
    }
  }

  /**
   * Lift an operation into a given monad.
   *
   * @tparam R the return type of the operation.
   * @param monadType the monad type to lift the operation into.
   * @param recType the type of the receiver of the operation.
   * @param opName the name of the operation to lift.
   * @param args the arguments of the operation to lift.  The first
   *   argument must be the receiver of the method call.
   * @param targs the type arguments of the operation to lift.
   * @param ropName the name of an alternate operation to use on deep
   *   receivers (i.e. receivers of type M[M[_]]).  This is an
   *   optional parameter: if its value is `None`, the value of
   *   `opName` will be used (default); if its value is `Some(n)`, `n`
   *   will be used.
   */
  def liftOp[R](c: Context)(monadType: c.Type)(
    opName: String, args: List[c.Expr[Any]] = Nil, targs: List[c.Type] = Nil,
    ropName: Option[String] = None): c.Expr[R] = {

    import c.universe._

    val lb = new LifterBundle[c.type](c) { val monad = monadType }

    // Get the receiver and argument types from the corresponding
    // trees and determine the monadic depth of the receiver type.
    val recType = args.head.actualType
    val recDepth = lb.monadDepth(recType)
    val args1 = args.toList.map(a => (a.tree, a.actualType))
    val mtdName = if (recDepth <= 1) opName else ropName match {
      case Some(on) => on
      case None     => opName
    }

    // Get the method name: `mtdName` may contain punctuation
    // characters, so use the parser to convert it into a valid name.
    val Ident(mtd) = c.parse(mtdName)

    // Lift the method call
    val liftedTree = lb.lift(mtd, recDepth, args1, targs)
    println(s"=== LIFTED METHOD CALL (${c.enclosingPosition})")
    println("== Before expansion: " + c.macroApplication)

    println(s"== Before typing:\n$liftedTree")

    // Type check the tree to catch lifting errors
    val typedTree = lb.typeCheck(liftedTree)
    println(s"== After typing:\n$typedTree")

    // The compiler might transform the lifted tree further before
    // re-typing it.  These transformations might introduce new terms
    // that aren't typed yet, leading to a partially typed tree.  To
    // make sure the transformed tree will be properly re-typed, we
    // need to reset symbols and attributes of the tree we return.
    c.Expr[R](c.resetAllAttrs(typedTree))
  }

  /** Dynamic macro handler. */
  private object Handler extends DynamicMacro.Handler {

    /** Handler for `applyDynamic` calls. */
    override def applyDynamic[M: c.WeakTypeTag](c: Context)(
      name: String, args: List[c.Expr[Any]], targs: List[c.Type]): c.Expr[M] = {
      liftOp(c)(implicitly[c.WeakTypeTag[M]].tpe)(name, c.prefix :: args, targs)
    }

    // FIMXE: Implement handler for `applyDynamicNamed` calls.

    /** Handler for `selectDynamic` calls. */
    override def selectDynamic[M: c.WeakTypeTag](c: Context)(
      name: String, targs: List[c.Type]): c.Expr[M] = {
      liftOp(c)(implicitly[c.WeakTypeTag[M]].tpe)(name, List(c.prefix), targs)
    }
  }
}

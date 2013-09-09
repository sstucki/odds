package ch.epfl.lamp.odds
package macros

import language.experimental.macros

import reflect.macros.Context

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
 * @todo FIXME: SI-7776 prevents implementation of type parameters.
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

  /**
   * Convenience class for bundling macro implementations.
   *
   * @tparam C the type of the context `c`.
   * @param c The context of the macro call associated with this bundle.
   */
  abstract class LifterBundle[C <: Context](val c: C) {

    import c.universe._

    val monad: c.Type

    /** Construct a monad instance. */
    def monadInst(inner: c.Type): c.Type = {
      appliedType(monad.typeConstructor, List(inner))
    }

    /** Get the type argument of a monad type. */
    def monadTypeArg(monadInstance: c.Type): c.Type = {
      val mBase = monadInstance.baseType(monad.typeSymbol)
      mBase match {
        case TypeRef(_, _, List(targ)) => targ
        //case _ => throw new java.lang.IllegalArgumentException(
        case _ => c.abort(c.enclosingPosition,
          "attempt to get type argument of non-type-reference type '" +
          mBase + "'")
      }
    }

    /** Get the number of nested monadic layers of a monad type. */
    def monadDepth(monadInstance: c.Type): Int = {
      val monadAny = monadInst(typeOf[Any])

      def loop(mInst: c.Type, depth: Int): Int =
        if (mInst <:< monadAny) loop(monadTypeArg(mInst), depth + 1)
        else depth

      loop(monadInstance, 0)
    }

    /** Get the type class instance of `MonadPlus` for a given monad. */
    def monadClassInst: c.Tree = {
      class Dummy[+A]
      val monadPlusCons = weakTypeOf[MonadPlus[Dummy]].typeConstructor
      c.inferImplicitValue(
        appliedType(monadPlusCons, List(monad.typeConstructor)), false)
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
     * @param args the arguments to the call and their types.  The first
     *   argument must be the receiver object of the method call.
     * @param targs the type arguments of the call.
     * @param rtype the expected monadic return type of the resulting
     *   call.
     * @param nrtype the expected result type of the original call
     *   (optional).  If `nrtype` is `NoType`, the method will try to
     *   infer the return type.
     * @return the tree of the lifted call.
     */
    def lift(
      mtd: c.Name, args: List[(c.Tree, c.Type)], targs: List[c.Type],
      rtype: c.Type, nrtype: c.Type = NoType): c.Tree = {

      def mkCall(mtd: c.Name, args: List[c.Tree], targs: List[c.Type]) = {
        val callee = Select(args.head, mtd)
        val mtdArgs = args.tail
        val calleeAndTypes =
          if (targs.isEmpty) callee
          else TypeApply(callee, targs.map(ta => TypeTree(ta)))
        if (mtdArgs.isEmpty) calleeAndTypes   // Selection
        else Apply(calleeAndTypes, mtdArgs)   // Application
      }

      val monadAny = monadInst(typeOf[Any])
      val (mtdArgs, bindArgss) = args.map { case (arg, atype) =>
        // Compute the types for the closure arguments and the terms
        // to pass to the corresponding `bind`/`fmap` calls.  If the
        // arguments already are of monad type `M[A]` extract `A`,
        // otherwise don't lift the corresponding argument but pass it
        // directly to the nested call.
        val atypeWidened = atype.widen
        if (atypeWidened <:< monadAny) {
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
        q"$mClassInst.unit{$nCall}"
      } else {                                  // Lift arguments
        val retDepth = monadDepth(rtype)

        val nRetType = if (nrtype != NoType) nrtype else {
          // At this point, we need to infer the return type of
          // `nCall` in order to decide whether the innermost
          // monadic option should be a `bind` (if the return type
          // is deep enough) or an `fmap` (if the return type is
          // shallower than the expected overall return type).
          // Ideally, we'd just type check `nCall` to infer its
          // result type.  However, some of the method arguments in
          // `nCall` are free (and untyped) identifiers, which might
          // lead to the type checker not being able to infer the
          // return type correctly (e.g. because we might be calling
          // an overloaded method).  So instead, we build a dummy
          // function binding (and typing) the free identifiers and
          // performing `nCall` internally.  We can then type check
          // that dummy function and get the return type of the
          // internal `nCall`.
          val (dummyArgs, dummyArgNames) = mtdArgTypes.map { atype =>
            val aname = newTermName(c.fresh("x$"))
            val arg = ValDef(Modifiers(Flag.PARAM), aname, TypeTree(atype),
              EmptyTree)
            (arg, Ident(aname))
          }.unzip
          val dummyCall = mkCall(mtd, dummyArgNames, targs)
          val dummyFun = q"((..$dummyArgs) => $dummyCall)"
          println(dummyFun)
          val Function(_, typedCall) = c.typeCheck(dummyFun)
          typedCall.tpe
        }
        val nRetDepth = monadDepth(nRetType)

        val (bindArgs1, nCall1) = if (nRetDepth >= retDepth) {
          // `nCall` returns deep result, use `bind`s all the way in.
          (bindArgs, nCall)
        } else {
          // `nCall` returns shallow result, wrap it into an `fmap`.
          val (atype1, aterm1, aname1) = bindArgs.head
          val fmap =
            q"$mClassInst.fmap{ ($aname1: $atype1) => $nCall }($aterm1)"
          (bindArgs.tail, fmap)
        }

        // Since `nCall1` might have been (partially) type checked
        // already, we need to reset its symbols and types so the type
        // checker will properly recheck it after macro expansion.
        val nCall2 = c.resetAllAttrs(nCall1)

        // Add `bind`s for the remaining arguments.
        (bindArgs1 :\ nCall2) {
          case ((atype, aterm, aname), call) =>
            q"$mClassInst.bind{ ($aname: $atype) => $call }($aterm)"
        }
      }
    }
  }

  /** Dynamic macro handler. */
  private object Handler extends DynamicMacro.Handler {

    /** Handler for `applyDynamic` calls. */
    override def applyDynamic[M](c: Context)(name: String)(
      args: List[c.Tree])(targs: List[c.Type]): c.Expr[M] = {

      import c.universe._
      val lb = new LifterBundle[c.type](c) { val monad = targs.head }

      val Ident(mtd) = c.parse(name)
      val rterm = c.prefix.tree
      val args1 = (rterm, rterm.tpe) :: args.map(a => (a, a.tpe))
      c.Expr(lb.lift(mtd, args1, targs.tail, rterm.tpe))
    }

    // FIMXE: Implement handler for `applyDynamicNamed` calls.

    /** Handler for `selectDynamic` calls. */
    override def selectDynamic[M](c: Context)(name: String)(
      targs: List[c.Type]): c.Expr[M] = {

      import c.universe._
      val lb = new LifterBundle[c.type](c) { val monad = targs.head }

      val Ident(member) = c.parse(name)
      val rterm = c.prefix.tree
      val receiver = List((rterm, rterm.tpe))
      c.Expr(lb.lift(member, receiver, targs.tail, rterm.tpe))
    }
  }
}


/**
 * Type class for lifting products into the monadic domain.
 *
 * ==For categorists==
 * This type class essentially represents the ''strength'' (and
 * ''costrength'') of the monad `M`, in that it allows lifting
 * products `(A, M[B])` to `M[(A, B)]`.  It also covers the
 * generalized case for finite products.
 *
 * @tparam M the monad type.
 * @tparam P the product type before lifting.
 */
trait MonadicProduct[M[+_], P <: Product] {

  /**
   * The underling product type of the monadic product represented by
   * the this type class instance.
   */
  type Inner <: Product

  /**
   * Lift a product into the monadic domain.
   *
   * @tparam P the product type before lifting.
   * @param p the product to lift.
   * @return A product of type `M[Inner]` where `Inner` is the
   *   underling product type of monad instances represented by this
   *   type class instance.
   */
  def lift(p: P): M[Inner]
}

/** Companion object of [[MonadicProduct]] trait. */
object MonadicProduct {

  /**
   * Fallback instance of [[MonadicProduct]] for trivial lifting.
   *
   * The underlying product type `Inner` is just the product type `P`
   * prior to lifting, and the lifting operation is just the monadic
   * `unit` operation.
   */
  final class Trivial[M[+_], P <: Product](implicit mp: MonadPlus[M])
      extends MonadicProduct[M, P] {
    type Inner = P
    def lift(p: P): M[P] = mp.unit(p)
  }

  /**
   * Materialize a type class instance of [[MonadicProduct]] for the
   * given monad and product type.
   *
   * @tparam M the monad type to lift products to.
   * @tparam P the product type before lifting.
   */
  implicit def materialize[M[+_], P <: Product]: MonadicProduct[M, P] =
    macro materializeImpl[M, P]

  /** Macro implementation of [[MonadicProduct.materialize]]. */
  def materializeImpl[M[+_], P <: Product](c: Context)(
    implicit mt: c.WeakTypeTag[M[_]], pt: c.WeakTypeTag[P])
      : c.Expr[MonadicProduct[M, P]] = {

    val mb = new MaterializerBundle[c.type, M, P](c) {
      val monad = mt.tpe
      val ptype = pt.tpe
    }
    mb.materialize
  }

  /**
   * Convenience class for bundling macro implementations.
   *
   * @tparam C the type of the context `c`.
   * @tparam M the monad type to lift products to.
   * @tparam P the product type before lifting.
   * @param c The context of the macro call associated with this bundle.
   */
  abstract class MaterializerBundle[C <: Context, M[+_], P <: Product](
    val c: C) {

    import c.universe._

    val monad: c.Type
    val ptype: c.Type

    lazy val lb = new MonadLifter.LifterBundle[c.type](c) {
      val monad = MaterializerBundle.this.monad
    }

    /** Materialize an instance of [[MonadicProduct.Trivial]]. */
    def materializeTrivial: c.Expr[MonadicProduct[M, P]] = {
      val triv = appliedType(
        weakTypeOf[Trivial[M, P]].typeConstructor,
        List(monad, ptype))
      c.Expr(q"new $triv")
    }

    /**
     * Materialize a type class instance of [[MonadicProduct]] for the
     * given monad and product type.
     */
    def materialize: c.Expr[MonadicProduct[M, P]] = {

      // If this product type is not a case class, fall back to trivial
      // lifting (i.e. through the monadic `unit` operation).
      val ptypeSym = ptype.typeSymbol
      if (!ptypeSym.isClass || !ptypeSym.asClass.isCaseClass) {
        materializeTrivial
      } else {

        // Get the symbol of the companion object and the names/types of
        // the fields of the product class.
        val pcompSym = ptype.typeSymbol.asClass.companionSymbol
        val pfields = ptype.declarations.toList.collect {
          case f: TermSymbol if f.isVal && f.isCaseAccessor =>
            (newTermName(f.name.toString.trim), f.typeSignatureIn(ptype))
        }

        // Find the "deepest" field type.  If the depth of that field
        // is 0, then none of the fields are monadic, and we can fall
        // back to trivial lifting.  Otherwise, use the type of that
        // field to compute an upper bound of the result type of the
        // `lift` method.
        val depths = pfields.map { f => (f._2, lb.monadDepth(f._2)) }
        val deepest = if (depths.isEmpty) (NoType, 0) else depths maxBy (_._2)
        if (deepest._2 == 0) materializeTrivial else {

          val retType = appliedType(deepest._1.typeConstructor, List(typeOf[Any]))

          // Compute the arguments (and their types) for the call to
          // the underlying product factory method from the fields of
          // the product class.
          val pname = newTermName(c.fresh("p$"))
          val (pargs, paterms) = pfields.map {
            case (fname, ftype) =>
              (ValDef(Modifiers(Flag.PARAM), fname, TypeTree(ftype), EmptyTree),
                (q"$pname.$fname", ftype))
          }.unzip

          // Lift the call to the product factory method.
          val call = lb.lift(
            newTermName("apply"),
            (Ident(pcompSym), pcompSym.typeSignature) :: paterms,
            Nil, retType)

          // Type check the lifted call to the factory method in order
          // to get the return type of the `lift` method and the
          // `Inner` type we are about to materialize.
          val dummy = q"(($pname: $ptype) => $call)"
          val Function(_, typedCall) = c.typeCheck(dummy)
          val callType = typedCall.tpe
          val innerType = lb.monadTypeArg(typedCall.tpe)

          // Since `call` has just been type checked, we need to reset
          // its symbols and types so the type checker will properly
          // recheck it after macro expansion.
          val call1 = c.resetAllAttrs(call)

          // Materialize the `lift` method and instantiate the type
          // class.
          val liftMtd = q"def lift($pname: $ptype): $callType = $call1"
          val parent = appliedType(
            weakTypeOf[MonadicProduct[M, P]].typeConstructor,
            List(monad, ptype))
          c.Expr(q"new $parent { type Inner = $innerType; $liftMtd }")
        }
      }
    }
  }
}

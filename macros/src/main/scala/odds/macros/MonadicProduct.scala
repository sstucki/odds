package ch.epfl.lamp.odds
package macros

import reflect.macros.Context

import functors.MonadPlus


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
    implicit mt: c.WeakTypeTag[M[Any]], pt: c.WeakTypeTag[P])
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
        val depths = pfields.map { f => lb.monadDepth(f._2) }
        val maxDepth = if (depths.isEmpty) 0 else depths.max
        if (maxDepth == 0) materializeTrivial else {

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
            newTermName("apply"), maxDepth,
            (Ident(pcompSym), pcompSym.typeSignature) :: paterms)

          // Type check the lifted call to the factory method in order
          // to get the return type of the `lift` method and the
          // `Inner` type we are about to materialize.
          val dummy = q"(($pname: $ptype) => $call)"
          val Function(_, typedCall) = lb.typeCheck(dummy)
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

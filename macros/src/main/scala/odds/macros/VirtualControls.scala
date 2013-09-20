package ch.epfl.lamp.odds
package macros

import internal.OddsBase

/**
 * Virtualization of control structures.
 *
 * This object provides macro definitions for the virtualized control
 * structures (as provided by Scala virtualize) that lift the
 * corresponding method calls into the probability monad `Rand[_]`.
 *
 * Example: When faced with an expression of the form `if (x) y else
 * z`, the Scala Virtualized compiler (or the `@virtualized` macro
 * annotation) will generate a method call: `__ifThenElse(x, y, z)`.
 * This method call will be bound to an implementation based on normal
 * rules of scoping.  If it binds to the one in this trait, the
 * corresponding macro will rewrite the call to e.g.
 * {{{
 *   Rand.bind(x) { x0: Boolean =>
 *     if (x0) y else z
 *   }
 * }}}
 * if `x` is of type `Rand[Boolean]` and `y`, `z` are of type
 * `Rand[A]` (for some type `A`), respectively.
 */
trait VirtualControls extends EmbeddedControls { this: OddsBase =>

  import VirtualControls.MacrosInOddsBase._

  // NOTE: Some of the signatures below have "by-val" arguments where
  // one would expect "by-name" arguments.  However, since these are
  // all macros the difference is irrelevant.  Furthermore, there's
  // currently a bug precluding the use of "by-name" parameters in
  // macros (See [[https://issues.scala-lang.org/browse/SI-5778]]).

  // Virtualized `if` statement.
  def __ifThenElse[A](cond: Any, tb: A, fb: A): A =
    macro ifThenElse[A]
  override def __ifThenElse[A](cond: => Boolean, tb: => A, fb: => A): A =
    macro ifThenElse[A]
}

/**
 * Companion object of [[VirtualControls]].
 *
 * This object provides macro implementations for the virtualized
 * control structures (as provided by Scala virtualize) that lift the
 * corresponding method calls into the probability monad `Rand[_]`.
 */
object VirtualControls {

  import reflect.macros.Context
  import MonadLifter._

  /**
   * Lift an `if` statement into the monadic domain.
   *
   * @tparam A the result type of the `if` statement.
   * @param c the context of the macro call.
   * @param monadType the monad type to lift the `if` statement into.
   * @param cond the condition of the `if` statement.
   * @param tb the `true` branch of the `if` statement.
   * @param fb the `false` branch of the `if` statement.
   */
  def liftIfThenElse[A](c: Context)(monadType: c.Type)(
    cond: c.Expr[Any], tb: c.Expr[A], fb: c.Expr[A]): c.Expr[A] = {

    import c.universe._
    val lb = new LifterBundle[c.type](c) { val monad = monadType }

    // Get the type and monadic depth of the condition.  If the depth
    // is non-zero, we actually need to lift the `if` statement.  In
    // that case allocate a new name for the nested condition.
    val condTree = cond.tree
    val condType = condTree.tpe
    val condDepth = lb.monadDepth(condType)
    val (condTerm, nCondName) = if (condDepth > 0) {
      val n = newTermName(c.fresh("c$"))
      (Ident(n), Some(n))
    } else (condTree, None)
    println(s"=== LIFTED IF (${c.enclosingPosition}):")
    // println("== Enclosing class:\n" + c.enclosingClass)
    println("== Before expansion: " + c.macroApplication)

    // Build the nested `if` statement.  If the condition is deep,
    // we'll need to further lift the nested call, so generate a call
    // to this macro.
    val tbTree = tb.tree
    val fbTree = fb.tree
    val ifTree = if (condDepth > 1) {
      // Deep condition, lift recursively.
      q"__ifThenElse($condTerm, $tbTree, $fbTree)"
    } else {
      // Shallow condition.
      If(condTerm, tbTree, fbTree)
    }

    val liftedTree = if (condDepth == 0) {
      ifTree    // No lifting required
    } else {
      val mClassInst = lb.monadClassInst
      val cn = nCondName.get
      val ct = lb.monadTypeArg(condType)
      val tbDepth = lb.monadDepth(tbTree.tpe)
      val fbDepth = lb.monadDepth(fbTree.tpe)
      assert(tbDepth == fbDepth)
      if (tbDepth < condDepth) {
        // Branches return shallow result, lift using `fmap`.
        q"$mClassInst.fmap($condTree) { $cn: $ct => $ifTree }"
      } else {
        // Branches return deep result, lift using `bind`.
        q"$mClassInst.bind($condTree) { $cn: $ct => $ifTree }"
      }
    }
    println(s"== Before typing:\n$liftedTree")

    // Type check the tree to catch lifting errors
    val typedTree = lb.typeCheck(liftedTree)
    println(s"== After typing:\n$typedTree")

    // The compiler might transform the lifted tree further before
    // re-typing it.  These transformations might introduce new terms
    // that aren't typed yet, leading to a partially typed tree.  To
    // make sure the transformed tree will be properly re-typed, we
    // need to reset symbols and attributes of the tree we return.
    c.Expr[A](c.resetAllAttrs(typedTree))
  }

  // The macro implementations in this object may only be called from
  // the companion trait (or subclasses thereof), to make sure the
  // prefix has type `internal.OddsBase`.
  private object MacrosInOddsBase {

    def ifThenElse[A](c: Context)(
      cond: c.Expr[Any], tb: c.Expr[A], fb: c.Expr[A]): c.Expr[A] = {

      import c.universe._

      // Get the `Rand[_]` type from the prefix (which we know to be
      // of type `internal.OddsBase`).
      val oddsIntfType = c.prefix.actualType
      val randTypeSym = oddsIntfType.member(newTypeName("Rand")).asType
      val randType = typeRef(oddsIntfType, randTypeSym, List(typeOf[Any]))

      // Do the actual lifting.
      liftIfThenElse(c)(randType)(cond, tb, fb)
    }
  }
}

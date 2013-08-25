package odds
package functors

/**
 * MonadPlus type class.
 *
 * Every instance of the Odds probability monad `Rand` needs to
 * provide a corresponding instance of this type class.
 *
 * ==Monad Laws==
 * Implementations of the methods in [[MonadPlus]] should fulfill the
 * following laws (for some equivalence relation `==`):
 * {{{
 *   fmap id                     ==  id
 *   fmap (g compose f)          ==  fmap(g) compose fmap(g)
 *
 *   unit compose f              ==  fmap(f) compose unit
 *   join compose fmap(fmap(f))  ==  fmap(f) compose join
 *
 *   join compose fmap(unit)     ==  join compose unit   ≅  id
 *   join compose fmap(join)     ==  join compose join
 * }}}
 *
 * The first two laws are just the functor laws for `M`, the second
 * pair are the naturality laws for `unit` and `join`, and the last
 * pair are the actual coherence conditions for monads.
 *
 * If an instance of this class overrides the derived `bind` method
 * (monadic extension), they should conform to the following laws:
 * {{{
 *   bind(f) compose unit  ==  f
 *   bind(unit)            ==  id
 * }}}
 * which can easily be derived from the above laws and the following
 * additional equivalence characterizing monadic extension.
 * {{{
 *   bind  ==  join compose fmap
 * }}}
 *
 * ===Monoidal laws===
 * Instances `M[A]` of the monad also form a monoid with operation
 * `plus` and identity `zero`.  Hence they should conform to the usual
 * identity and associativity laws for monoids:
 * {{{
 *   mx plus zero          ==  mx
 *   zero plus mx          ==  mx
 *
 *   (mx plus my) plus mz  ==  mx plus (my plus mz)
 * }}}
 *
 * Functions `M[A] => M[B]` between instances are monoid
 * homomorphisms, and should conform to the corresponding laws:
 * {{{
 *   f(zero)        ==  mzero
 *   f(mx plus my)  ==  f(mx) plus f(my)
 * }}}
 * In particular, this implies the following derived laws for
 * functions `A => M[B]` (that is, Kleisli morphisms):
 * {{{
 *   bind(f)(mzero)        ==  mzero
 *   bind(x => mzero)(mx)  ==  mzero
 *   bind(f)(mx plus my)   ==  bind(f)(mx) plus bind(f)(my)
 * }}}
 *
 * ==For functional programmers==
 * This is a type class representing what is commonly referred to as
 * an "additive monad" in functional programming.  That is, a monad
 * with a monoid structure (given by `plus` and `zero`).  Every
 * collection in the Scala collection API is a monad with a monoid
 * structure, and consequently this class provides an implicit
 * conversion decorating its abstract monad functor type `M` with the
 * basic collection operations (see [[MonadPlus.ToScalaMonadic]]).
 *
 * This class corresponds roughly to the `MonadPlus` type class in
 * Haskell.
 *
 * ==For categorists==
 * This class corresponds to a strong monad (or triple) `(M, unit,
 * join)` on the category of Scala types, with a tensorial strength
 * for the Cartesian product.  In addition, the image `M[A]` of an
 * object (type) `A` is a monoid `(M[A], plus, zero)`.  Although this
 * is called an "additive monoid" in the context of functional
 * programming, the functor `M` need not be additive, and hence this
 * is not an additive monad in the categorical sense.
 *
 * @tparam M the Scala type representing the functor of the monad.
 */
trait MonadPlus[M[+A]] {

  /** The Scala type representing the functor of the monad. */
  type FType[+A] = M[A]

  /**
   * Functor application (morphisms).
   *
   * ==For functional programmers==
   * This corresponds to the `map` operation in Scala collections, in
   * Haskell it corresponds to the `fmap` operation of the `Functor`
   * class.
   *
   * ==For categorists==
   * This corresponds to the mapping (on morphisms) defined by the
   * functor `M` of the monad `(M, unit, join)`.
   */
  def fmap[A, B](f: A => B)(mx : M[A]): M[B]

  /**
   * Monadic unit.
   *
   * ==For functional programmers==
   * This is the monadic 'unit' (or 'return') operation.  In the Scala
   * collection API, this corresponds to `M(x)` (i.e. passing a single
   * argument to the `apply` method of the companion object).  In
   * Haskell it corresponds to the `return` operation of the `Monad`
   * class.
   *
   * ==For categorists==
   * This corresponds to a natural transformation `unit: I => M` from
   * the identity functor `I` to `M` in the monad `(F, unit, join)`.
   */
  def unit[A](x: A): M[A]

  /**
   * Monadic multiplication.
   *
   * ==For functional programmers==
   * This is monadic 'join' operation.  In the Scala collection API,
   * this corresponds to `flatten` method of `F`.  In Haskell it
   * corresponds to the `join` operation of the `Monad` class.
   *
   * ==For categorists==
   * This corresponds to a natural transformation `join: M M => M`
   * from the square of `M` to `M` in the monad `(M, unit, join)`.
   */
  def join[A](mmx: M[M[A]]): M[A]

  /**
   * Monadic bind (extension).
   *
   * ==For functional programmers==
   * This is monadic 'bind' operation.  In the Scala collection API,
   * this corresponds to `flatMap` method of `F`.  In Haskell it
   * corresponds to the `(>>=)` operation of the `Monad` class.
   *
   * ==For categorists==
   * This corresponds to monadic extension operation.  It is related
   * to the Kleisli composition `g * f` of two morphisms `f: X -> M Y`
   * and `g: Y -> M Z` as follows: `bind(g) . f = g * f`.
   */
  def bind[A, B](f: A => M[B])(mx: M[A]): M[B] = join(fmap(f)(mx))

  /**
   * Monoidal unit (monadic 'zero').
   *
   * ==For functional programmers==
   * This is the monadic 'zero'.  In the Scala collection API, this
   * corresponds to, an empty collection, i.e. `M.empty` or the `M()`
   * (calling the `apply` method of the companion object without an
   * argument).  In Haskell it corresponds to the `zero` operation
   * of the `MonadPlus` class.
   *
   * ==For categorists==
   * This corresponds the identity of the Monoid `M A`.
   */
  def zero[A]: M[A]

  /**
   * Monoidal tensor product (monadic 'plus').
   *
   * ==For functional programmers==
   * This is the monadic 'plus' operation.  In the Scala collection
   * API, this corresponds to the `++` or `orElse` method of `M`.  In
   * Haskell it corresponds to the `plus` operation of the `MonadPlus`
   * class.
   *
   * ==For categorists==
   * This corresponds the tensor product of the Monoid `M A`.
   */
  def plus[A](m1: M[A], m2: M[A]): M[A]

  /** Implicit view as a Scala collection. */
  implicit final class ToScalaMonadic[+A](mx: M[A]) {

    //@inline def ++[B >: A](my: M[B]): M[B] = plus(mx, my)
    @inline def map[B](f: A => B): M[B] = fmap(f)(mx)
    @inline def filter(p: A => Boolean): M[A] = bind { x: A =>
      if (p(x)) unit(x) else zero
    } (mx)
    @inline def flatMap[B](f: A => M[B]): M[B] = bind(f)(mx)
    @inline def foreach[U](f: A => U): Unit = fmap(f)(mx)
    @inline def orElse[B >: A](my: M[B]): M[B] = plus(mx, my)
    @inline def withFilter(p: A => Boolean): WithFilter =
      new WithFilter(p)

    /** View for `withFilter` method. */
    final class WithFilter(p: A => Boolean) {
      //@inline def ++[B >: A](my: M[B]): M[B] = plus(mx filter p, my)
      @inline def map[B](f: A => B): M[B] = fmap(f)(mx filter p)
      @inline def flatMap[B](f: A => M[B]): M[B] = bind(f)(mx filter p)
      @inline def foreach[U](f: A => U): Unit = fmap(f)(mx filter p)
      @inline def orElse[B >: A](my: M[B]): M[B] = plus(mx filter p, my)
      @inline def withFilter(q: A => Boolean): WithFilter =
        new WithFilter(x => p(x) && q(x))
    }
  }
}

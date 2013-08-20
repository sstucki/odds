package odds
package functors


/** Probability Monad type class. */
trait ProbMonad[M[+A], D[+A]] extends MonadPlus[M] {

  /** Random variable type. */
  type Rand[+A] = M[A]

  /** Abstract distribution type. */
  type Dist[+A] = D[A]

  /** Make a probabilistic choice. */
  def choose[A](xs: Dist[A]): M[A]

}

/**
 * Companion object of [[ProbMonad]].
 *
 * Allows for easy import of monadic operations into scopes containing
 * an (implicit) instance of [[ProbMonad]].
 */
object ProbMonad {
  @inline def fmap[A, B, M[+_], D[+_]](f: A => B)(mx: M[A])(
    implicit m: ProbMonad[M, D]): M[B] = m.fmap(f)(mx)
  @inline def unit[A, M[+_], D[+_]](x: A)(implicit m: ProbMonad[M, D]): M[A] =
    m.unit(x)
  @inline def join[A, M[+_], D[+_]](mmx: M[M[A]])(
    implicit m: ProbMonad[M, D]): M[A] = m.join(mmx)
  @inline def bind[A, B, M[+_], D[+_]](f: A => M[B])(mx: M[A])(
    implicit m: ProbMonad[M, D]): M[B] = m.bind(f)(mx)
  @inline def zero[A, M[+_], D[+_]](implicit m: ProbMonad[M, D]): M[A] =
    m.zero[A]
  @inline def plus[A, M[+_], D[+_]](m1: M[A], m2: M[A])(
    implicit m: ProbMonad[M, D]): M[A] = m.plus(m1, m2)
  @inline def choose[A, M[+_], D[+_]](xs: D[A])(
    implicit m: ProbMonad[M, D]): M[A] = m.choose(xs)
}

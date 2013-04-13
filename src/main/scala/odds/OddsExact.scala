package odds

/** Simple, exact inference for the ODDS language. */
trait OddsExact extends OddsIntf with DistIterables with CommittedChoices {

  type Rand[+A] = RandVar[A]

  abstract class RandVar[+A] extends RandIntf[A] {

    def flatMap[B](f: A => Rand[B]): Rand[B] = RandVarFlatMap(this, f)

    def orElse[B >: A](that: Rand[B]): Rand[B] = RandVarOrElse(this, that)

    def reify0[B](p: Prob)(cont: (A, Prob) => Dist[B]): Dist[B]

    /**
     * Reify a random variable representing a probabilistic computation.
     *
     * @return the distribution over the values of this random variable.
     */
    def reify: Dist[A] = {
      consolidate(reify0(1){ (x, p) => Iterable(x -> p) })
    }
  }

  final case class RandVarChoice[+A](dist: Dist[A])
      extends RandVar[A]
      with CommittedChoice[A] {

    def reify0[B](p: Prob)(cont: (A, Prob) => Dist[B]): Dist[B] =
      choice match {
        case Some(v) => {
          assert(dist exists (_._1 == v), v + " not in " + dist)
          cont(v, p)
        }
        case None => {
          val d = dist flatMap { case (v, q) =>
              commit(v)
              cont(v, p * q)
          }
          relax
          d
        }
      }
  }

  final case class RandVarFlatMap[+A, B](x: RandVar[B], f: B => Rand[A])
    extends RandVar[A] {

    def reify0[T](p: Prob)(cont: (A, Prob) => Dist[T]): Dist[T] =
      x.reify0(p){ (y, q) => f(y).reify0(q)(cont) }
  }

  final case class RandVarOrElse[+A](x: RandVar[A], y: RandVar[A])
      extends RandVar[A] {

    def reify0[B](p: Prob)(cont: (A, Prob) => Dist[B]): Dist[B] =
      x.reify0(p)(cont) ++ y.reify0(p)(cont)
  }

  def choice[A](xs: (A, Prob)*): Rand[A] = RandVarChoice(xs)
}

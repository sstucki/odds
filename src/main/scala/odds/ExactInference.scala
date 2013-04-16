package odds

/** Simple, exact inference for the ODDS language. */
trait ExactInference extends OddsIntf with DistIterables {

  import CommittedChoice.Environment

  type Rand[+A] = RandVar[A]

  sealed abstract class RandVar[+A] extends RandIntf[A] {

    def flatMap[B](f: A => Rand[B]): Rand[B] = RandVarFlatMap(this, f)

    def orElse[B >: A](that: Rand[B]): Rand[B] = RandVarOrElse(this, that)

    def reify0[B](p: Prob, env: Environment)(
      cont: (A, Prob, Environment) => Dist[B]): Dist[B]

    /**
     * Reify a random variable representing a probabilistic computation.
     *
     * @return the distribution over the values of this random variable.
     */
    def reify: Dist[A] = {
      consolidate(reify0(1, Map()){ (x, p, e) => Iterable(x -> p) })
    }
  }

  final case class RandVarChoice[+A](dist: Dist[A])
      extends RandVar[A]
      with CommittedChoice[A] {

    def reify0[B](p: Prob, env: Environment)(
      cont: (A, Prob, Environment) => Dist[B]): Dist[B] =
      choice(env) match {
        case Some(v) => {
          assert(dist exists (_._1 == v), v + " not in " + dist)
          cont(v, p, env)
        }
        case None => dist flatMap {
          case (v, q) =>
            withChoice(env, v) { e =>
              cont(v, p * q, e)
            }
        }
      }
  }

  final case class RandVarFlatMap[+A, B](x: RandVar[B], f: B => Rand[A])
    extends RandVar[A] with CommittedChoice[Rand[A]] {

    def reify0[T](p: Prob, env: Environment)(
      cont: (A, Prob, Environment) => Dist[T]): Dist[T] =
      x.reify0(p, env){ (y, q, e) =>
        choice(env) match {
          case Some(r) => r.reify0(q, e)(cont)
          case None => {
            val r = f(y)
            withChoice(e, r) { e1 =>
              r.reify0(q, e1)(cont)
            }
          }
        }
      }
  }

  final case class RandVarOrElse[+A](x: RandVar[A], y: RandVar[A])
      extends RandVar[A] {

    def reify0[B](p: Prob, env: Environment)(
      cont: (A, Prob, Environment) => Dist[B]): Dist[B] =
      x.reify0(p, env)(cont) ++ y.reify0(p, env)(cont)
  }

  def choice[A](xs: (A, Prob)*): Rand[A] = RandVarChoice(xs)
}

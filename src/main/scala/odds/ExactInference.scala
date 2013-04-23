package odds

/** Simple, exact inference for the ODDS language. */
trait ExactInference extends OddsIntf with DistMaps {

  import CommittedChoice.Environment

  type Rand[+A] = RandVar[A]

  sealed abstract class RandVar[+A] extends RandIntf[A] {

    def flatMap[B](f: A => Rand[B]): Rand[B] = RandVarFlatMap(this, f)

    def orElse[B >: A](that: Rand[B]): Rand[B] = RandVarOrElse(this, that)
  }

  final case class RandVarChoice[+A](dist: Dist[A])
      extends RandVar[A] with CommittedChoice[A]
  final case class RandVarFlatMap[+A, B](x: RandVar[B], f: B => Rand[A])
      extends RandVar[A] with CommittedChoice[Rand[A]]
  final case class RandVarOrElse[+A](x: RandVar[A], y: RandVar[A])
      extends RandVar[A]

  def choice[A](xs: (A, Prob)*): Rand[A] = RandVarChoice(dist(xs: _*))

  /**
   * Reify a random variable representing a probabilistic computation.
   *
   * @return the distribution over the values of this random variable.
   */
  def reify[A](x: RandVar[A]): Dist[A] =
    consolidate(explore(x, 1, Map()){ (y, p, e) => dist(y -> p) })

  def explore[A, B](x: RandVar[A], p: Prob, env: Environment)(
    cont: (A, Prob, Environment) => Dist[B]): Dist[B] = x match {
    case x @ RandVarChoice(dist) => x.choice(env) match {
      case Some(v) => {
        assert(dist exists (_._1 == v), v + " not in " + dist)
        cont(v, p, env)
      }
      case None => dist flatMap {
        case (v, q) =>
          x.withChoice(env, v) { e =>
            cont(v, p * q, e)
          }
      }
    }
    case t @ RandVarFlatMap(x, f) => explore(x, p, env){ (y, q, e) =>
      t.choice(env) match {
        case Some(r) => explore(r, q, e)(cont)
        case None => {
          val r = f(y)
          t.withChoice(e, r) { e1 =>
            explore(r, q, e1)(cont)
          }
        }
      }
    }
    case RandVarOrElse(x, y) =>
      explore(x, p, env)(cont) ++ explore(y, p, env)(cont)
  }
}

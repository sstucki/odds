package odds

import scala.collection.mutable

/**
 * Odds interface with delayed-choice probability monad.
 *
 * This trait defines the [[Rand]] probability monad use in all
 * inference algorithms based on delayed evaluation.
 */
trait DelayedChoiceIntf extends OddsIntf with DistMaps {

  type Rand[+A] = RandVar[A]
  type Environment = CommittedChoice.Environment

  /**
   * The delayed-choice probability monad.
   *
   * This monad is used in all inference algorithms based on delayed
   * evaluation.  Its sub-classes represent nodes in a
   * tree-representation of the probabilistic computation.  The tree
   * can be walked during evaluation to reify the computation.
   */
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
   * The result of a local exploration of the search tree defined by a
   * random variable.
   *
   * @tparam A the type of the support of the random variable to be
   *         explored.
   * @param solutions a distribution over solutions discovered thus
   *        far.
   * @param branches a distribution over thunks for further exploring
   *        partially explored branches.
   */
  final case class ExploreRes[+A](
    solutions: Dist[A], branches: Dist[Prob => ExploreRes[A]])

  /** Locally explore the search tree defined by a random variable. */
  def explore[A](x: RandVar[A]): ExploreRes[A] = {

    def explore0[B](x: RandVar[B], p: Prob, env: Environment)(
      cont: (B, Prob, Environment) => ExploreRes[A]): ExploreRes[A] = x match {
      case x @ RandVarChoice(d) => x.choice(env) match {
        case Some(v) => {
          assert(d exists (_._1 == v), v + " not in " + d)
          cont(v, p, env)
        }
        case None => {
          // Don't count certainties as choices.
          val certainty = !d.isEmpty && d.tail.isEmpty
          if (certainty) {
            // Explore sub-branches and return result
            val (v, q) = d.head
            x.withChoice(env, v) { e =>
              cont(v, p * q, e)
            }
          } else {
            // Return branch thunks
            val branches = d map {
              case (v, q) => x.withChoice(env, v) { e =>
                (cont(v, _: Prob, e), p * q)
              }
            }
            ExploreRes(dist(), branches)
          }
        }
      }
      case t @ RandVarFlatMap(x, f) =>
        explore0(x, p, env){ (y, q, e) =>
          t.choice(env) match {
            case Some(r) => explore0(r, q, e)(cont)
            case None => {
              val r = f(y)
              t.withChoice(e, r) { e1 =>
                explore0(r, q, e1)(cont)
              }
            }
          }
        }
      case RandVarOrElse(x, y) => {
        val ExploreRes(xd, xe) = explore0(x, p, env)(cont)
        val ExploreRes(yd, ye) = explore0(y, p, env)(cont)
        ExploreRes(xd ++ yd, xe ++ ye)
      }
    }

    explore0(x, 1.0, Map()) {
      (y, q, e) => ExploreRes(dist(y -> q), dist())
    }
  }
}

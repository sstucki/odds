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
      extends RandVar[A] with CommittedChoice[A]
  final case class RandVarOrElse[+A](x: RandVar[A], y: RandVar[A])
      extends RandVar[A] with CommittedChoice[A]

  def choice[A](xs: (A, Prob)*): Rand[A] = RandVarChoice(dist(xs: _*))

  /**
   * The result of a local exploration of the search tree defined by a
   * random variable.
   *
   * @tparam A the type of the support of the explored random variable.
   * @param solutions a distribution over solutions discovered thus
   *        far.
   * @param branches a distribution over thunks for further exploring
   *        partially explored branches.
   * @param id the ID of the explored [[RandVar]] instance.
   */
  final case class ExploreRes[+A](
    solutions: Dist[A], branches: Dist[Prob => ExploreRes[A]], id: Long = -1)

  /** Locally explore the search tree defined by a random variable. */
  def explore[A](x: RandVar[A]): ExploreRes[A] = {

    type ContRes = (Either[A, Prob => ExploreRes[A]], Prob)

    def contToEploreRes(cr: ContRes): ExploreRes[A] = cr match {
      case (Left(v),  p) => ExploreRes(Dist(v -> p), Dist())
      case (Right(t), p) => t(p)
    }

    def explore0[B](x: RandVar[B], p: Prob, env: Environment)(
      cont: (B, Prob, Environment) => ContRes): ContRes = x match {
      case x @ RandVarChoice(d) => x.choice(env) match {
        case Some(v) => {
          assert(d exists (_._1 == v), v + " not in " + d)
          cont(v, p, env)
        }
        case None => {
          // Don't count certainties as choices.
          val certainty = !d.isEmpty && d.tail.isEmpty
          if (certainty) {
            // Continue exploring branch and return result
            val (v, q) = d.head
            x.withChoice(env, v) { e =>
              cont(v, p * q, e)
            }
          } else {
            // // Return branch thunks
            // val branches = d map {
            //   case (v, q) => x.withChoice(env, v) { e =>
            //     (cont(v, _: Prob, e), p * q)
            //   }
            // }
            // ExploreRes(dist(), branches, x.id)

            // Return thunk
            (Right({ p: Prob =>
              var vs = Dist[A]()
              var ts = Dist[Prob => ExploreRes[A]]()
              d foreach {
                case (v, q) => x.withChoice(env, v) { e =>
                  cont(v, p * q, e) match {
                    case (Left(v),  w) => vs += (v -> w)
                    case (Right(t), w) => ts += (t -> w)
                  }
                }
              }
              ExploreRes(vs, ts, x.id)
            }) -> p)
          }
        }
      }
      case t @ RandVarFlatMap(x, f) => t.choice(env) match {
        case Some(v) => cont(v, p, env)
        case None => explore0(x, p, env) { (y, q, e) =>
          explore0(f(y), q, e) { (z, w, e1) =>
            t.withChoice(e1, z)(cont(z, w, _))
          }
        }
      }
      case t @ RandVarOrElse(x, y) => t.choice(env) match {
        case Some(v) => cont(v, p, env)
        case None =>
          (Right({ p: Prob =>
            val xcr = explore0(x, p, env) {
              (z, q, e) => t.withChoice(e, z)(cont(z, q, _))
            }
            val ycr = explore0(y, p, env){
              (z, q, e) => t.withChoice(e, z)(cont(z, q, _))
            }
            val ExploreRes(xd, xb, _) = contToEploreRes(xcr)
            val ExploreRes(yd, yb, _) = contToEploreRes(ycr)
            ExploreRes(xd ++ yd, xb ++ yb, t.id)
          }) -> p)
      }
    }

    contToEploreRes(explore0(x, 1.0, Map()) {
      (y, q, e) => (Left(y) -> q)
    })
  }
}

package odds

import functors.ProbMonad
import scala.collection.mutable

/**
 * Delayed-choice probability monad.
 *
 * This trait contains the definition of the (base) type class
 * instance of the probability monad use in all inference algorithms
 * based on delayed evaluation.
 */
trait DelayedChoiceIntf extends OddsIntf with DistMaps {
  this: OddsLang =>

  /** Concrete random variable type. */
  type Rand[+A] = RandVar[A]

  //type Dist[+A] = DistMaps.this.Dist[A]

  type Environment = CommittedChoice.Environment

  /**
   * The delayed-choice probability monad.
   *
   * This monad is used in all inference algorithms based on delayed
   * evaluation.  Its sub-classes represent nodes in a
   * tree-representation of the probabilistic computation.  The tree
   * can be walked during evaluation to reify the computation.
   */
  abstract class RandVar[+A] extends RandIntf[A] with CommittedChoice[A]

  final case class RandVarChoose[+A](xs: Dist[A]) extends RandVar[A]
  final case class RandVarBind[+A, B](x: RandVar[B], f: B => RandVar[A])
      extends RandVar[A]
  final case class RandVarPlus[+A](x: RandVar[A], y: RandVar[A])
      extends RandVar[A]

  /**
   * Delayed-choice probability monad type class instance.
   *
   * This trait defines the (base) type class instance of the
   * probability monad use in all inference algorithms based on
   * delayed evaluation.
   */
  trait DelayedChoiceMonad extends ProbMonad[Rand, Dist] {
    @inline def fmap[A, B](f: A => B)(mx: Rand[A]) =
      RandVarBind(mx, { y: A => unit(f(y)) })
    @inline def unit[A](x: A) = choose(Dist(x -> 1.0))
    @inline def join[A](mmx: Rand[Rand[A]]): Rand[A] =
      RandVarBind(mmx, { y: Rand[A] => y })
    @inline override def bind[A, B](f: A => Rand[B])(mx: Rand[A]) =
      RandVarBind(mx, f)
    @inline def zero[A] = choose[A](Dist())
    @inline def plus[A](m1: Rand[A], m2: Rand[A]): Rand[A] =
      RandVarPlus(m1, m2)
    @inline def choose[A](xs: Dist[A]): Rand[A] = RandVarChoose(xs)
  }

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
      case x @ RandVarChoose(xs) => x.choice(env) match {
        case Some(v) => {
          assert(xs exists (_._1 == v), v + " not in " + xs)
          cont(v, p, env)
        }
        case None => {
          // Don't count certainties as choices.
          val certainty = !xs.isEmpty && xs.tail.isEmpty
          if (certainty) {
            // Continue exploring branch and return result
            val (v, q) = xs.head
            x.withChoice(env, v) { e =>
              cont(v, p * q, e)
            }
          } else {
            // Return thunk
            (Right({ p: Prob =>
              var vs = Dist[A]()
              var ts = Dist[Prob => ExploreRes[A]]()
              xs foreach {
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
      case t @ RandVarBind(x, f) => t.choice(env) match {
        case Some(v) => cont(v, p, env)
        case None => explore0(x, p, env) { (y, q, e) =>
          explore0(f(y), q, e) { (z, w, e1) =>
            t.withChoice(e1, z)(cont(z, w, _))
          }
        }
      }
      case t @ RandVarPlus(x, y) => t.choice(env) match {
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

object DelayedChoiceMonad extends DistMaps {
}

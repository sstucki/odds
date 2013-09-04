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
  this: OddsIntf =>

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

  final case class RandVarUnit[+A](x: A, p: Prob) extends RandVar[A]
  final case class RandVarFmap[+A, B](mx: RandVar[B], f: B => A)
      extends RandVar[A]
  final case class RandVarJoin[+A](mmx: RandVar[RandVar[A]]) extends RandVar[A]
  final case class RandVarBind[+A, B](mx: RandVar[B], f: B => RandVar[A])
      extends RandVar[A]
  final case object RandVarZero extends RandVar[Nothing]
  final case class RandVarPlus[+A](mx: RandVar[A], my: RandVar[A])
      extends RandVar[A]
  final case class RandVarChoose[+A](xs: Dist[A]) extends RandVar[A]

  /**
   * Delayed-choice probability monad type class instance.
   *
   * This trait defines the (base) type class instance of the
   * probability monad use in all inference algorithms based on
   * delayed evaluation.
   */
  trait DelayedChoiceMonad extends RandInternalIntf {

    // It is tempting to think we could use the monad laws to
    // simplify/normalize some trees built for the corresponding
    // stochastic computations, e.g. to automatically rewrite
    // instances of `RandVarFmap(RandVarFmap(x, g), f)` into
    // `RandVarFmap(f(g(x)))`.  However, the only optimizations we are
    // allowed to perform are those that replace an instance of
    // `RandVar` with another instance of `RandVar` that will commit
    // to the same choice during exploration.  This is because
    // instances of `RandVar` actually identify choices, hence
    // replacing them with another instance will remove information
    // about correlation between choices and alter the program
    // semantics.  Certain outcomes are an exception because they are
    // referentially transparent, i.e. they always correlate perfectly
    // with other certain outcomes of the same value, so it is safe to
    // replace them.

    def fmap[A, B](f: A => B)(mx: Rand[A]) = mx match {
      // By `fmap(f) compose unit  ==  unit compose f`
      case RandVarUnit(x, p) => unit(f(x), p)

      // By `fmap(f)(zero)  ==  mzero`
      case RandVarZero       => zero[B]
      case _                 => RandVarFmap(mx, f)
    }

    @inline private def unit[A](x: A, p: Prob) = RandVarUnit(x, p)
    @inline def unit[A](x: A)                  = unit(x, 1.0)

    def join[A](mmx: Rand[Rand[A]]): Rand[A] = mmx match {
      // By `join compose unit  ==  id`
      case RandVarUnit(x, 1.0) => x

      // By `join(zero)  ==  zero`
      case RandVarZero         => zero
      case _                   => RandVarJoin(mmx)
    }

    override def bind[A, B](f: A => Rand[B])(mx: Rand[A]) = mx match {
      // By `bind(f) compose unit  ==  f`
      case RandVarUnit(x, 1.0) => f(x)

      // By `bind(f)(mzero)  ==  zero`
      case RandVarZero         => zero
      case _                   => RandVarBind(mx, f)
      }

    @inline def zero[A] = RandVarZero

    def plus[A](m1: Rand[A], m2: Rand[A]): Rand[A] = (m1, m2) match {
      // By monoid identity
      case (RandVarZero, m) => m

      // By monoid identity
      case (m, RandVarZero) => m
      case _                => RandVarPlus(m1, m2)
    }

    def choose[A](xs: Dist[A]): Rand[A] = xs match {
      // Optimize certainties/singletons
      case Dist((x, p)) => unit(x, p)
      case _            => RandVarChoose(xs)
    }
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
    solutions: Dist[A], branches: Dist[Prob => ExploreRes[A]],
    id: CommittedChoice.Id)

  private val zeroExploreRes = ExploreRes(
    Dist[Nothing](), Dist[Prob => ExploreRes[Nothing]](), RandVarZero.id)

  /** Locally explore the search tree defined by a random variable. */
  def explore[A](x: RandVar[A]): ExploreRes[A] = {

    type ContRes = (Either[A, Prob => ExploreRes[A]], Prob)

    def contToEploreRes(cr: ContRes): ExploreRes[A] = cr match {
      case (Left(v),  p) => ExploreRes(Dist(v -> p), Dist(), x.id)
      case (Right(t), p) => t(p)
    }

    def explore0[B](mx: RandVar[B], p: Prob, env: Environment)(
      cont: (B, Prob, Environment) => ContRes): ContRes = mx match {
      case RandVarUnit(x, q) => cont(x, p * q, env)
      case fmp @ RandVarFmap(mx, f) => fmp.choice(env) match {
        case Some(x) => cont(x, p, env)
        case None => explore0(mx, p, env) { (x, q, e) =>
          val y = f(x)
          fmp.withChoice(e, y)(cont(y, q, _))
        }
      }
      case jin @ RandVarJoin(mmx) => jin.choice(env) match {
        case Some(x) => cont(x, p, env)
        case None => explore0(mmx, p, env) { (mx, q1, e1) =>
          explore0(mx, q1, e1) { (x, q2, e2) =>
            jin.withChoice(e2, x)(cont(x, q2, _))
          }
        }
      }
      case bnd @ RandVarBind(mx, f) => bnd.choice(env) match {
        case Some(x) => cont(x, p, env)
        case None => explore0(mx, p, env) { (x1, q1, e1) =>
          explore0(f(x1), q1, e1) { (x2, q2, e2) =>
            bnd.withChoice(e2, x2)(cont(x2, q2, _))
          }
        }
      }
      case RandVarZero => Right((_: Prob) => zeroExploreRes) -> p
      case pls @ RandVarPlus(mx, my) => pls.choice(env) match {
        case Some(x) => cont(x, p, env)
        case None =>
          (Right({ p: Prob =>
            val xcr = explore0(mx, p, env) {
              (x, q, e) => pls.withChoice(e, x)(cont(x, q, _))
            }
            val ycr = explore0(my, p, env){
              (y, q, e) => pls.withChoice(e, y)(cont(y, q, _))
            }
            val ExploreRes(xd, xb, _) = contToEploreRes(xcr)
            val ExploreRes(yd, yb, _) = contToEploreRes(ycr)
            ExploreRes(xd ++ yd, xb ++ yb, pls.id)
          }) -> p)
      }
      case chs @ RandVarChoose(xs) => chs.choice(env) match {
        case Some(x) => {
          assert(xs exists (_._1 == x), x + " not in " + xs)
          cont(x, p, env)
        }
        case None => {
          // Don't count certainties as choices.
          val certainty = !xs.isEmpty && xs.tail.isEmpty
          if (certainty) {
            // Continue exploring branch and return result
            val (x, q) = xs.head
            chs.withChoice(env, x) { e =>
              cont(x, p * q, e)
            }
          } else {
            // Return thunk
            Right({ p: Prob =>
              var vs = Dist[A]()
              var ts = Dist[Prob => ExploreRes[A]]()
              xs foreach {
                case (v, q) => chs.withChoice(env, v) { e =>
                  cont(v, p * q, e) match {
                    case (Left(v),  w) => vs += (v -> w)
                    case (Right(t), w) => ts += (t -> w)
                  }
                }
              }
              ExploreRes(vs, ts, chs.id)
            }) -> p
          }
        }
      }
    }
    contToEploreRes(explore0(x, 1.0, Map()) {
      (y, q, e) => (Left(y) -> q)
    })
  }
}

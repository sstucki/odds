package odds

import scala.collection.mutable

/**
 * Importance sampling with depth-bound look-ahead.
 *
 * This trait implements importance sampling as described in "Embedded
 * Probabilistic Programming" by Oleg Kiselyov and Chung-chieh Shan.
 */
trait LocalImportanceSampling extends OddsIntf with DistMaps {

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


  /** Pseudo-random number generator used for sampling. */
  //val prng = new java.util.Random

  /**
   * Approximate the distribution defined by a probabilistic
   * computation.
   *
   * @param samples the number of samples used to approximate the
   *        distribution.
   * @return an approximation of the distribution over the values
   *         of this random variable.
   */
  def sample[A](samples: Int, depth: Int, error: Prob = 0.0)(x: Rand[A]): Dist[A] = {
    var d: Dist[A] = dist()
    var solCount = 0;
    var runCount = 0;
    while (runCount < samples) {
      val sd = sample1(x, depth, error)
      d ++= sd
      solCount += sd.size
      runCount += 1
    }
    println("importance sampler: " + runCount + " samples, " + solCount + " solutions.")
    d
  }

  abstract class BranchClosure[+A] extends Function2[Prob, Int, ExploreRes[A]]
  type ExploreRes[+A] = (Dist[A], Dist[BranchClosure[A]])

  def sample1[A](x: RandVar[A], depth: Int, error: Prob): Dist[A] = {

    val initClos = new BranchClosure[A] {
      def apply(p, d) = explore(x, p, Map(), d) {
        (y, q, e, k) => (dist(y -> q), dist())
      }
    }
    var solutions: Dist[A] = dist()
    var branches: Dist[BranchClosure[A]] = dist(initClos -> 1)

    // Keep exploring candidate branches until none remain.
    while (!branches.isEmpty &&
      (branches.totalWeight != 0.0) &&  // Don't explore infinities
      (branches.totalWeight >= error)) {

      // Pick a branch at random
      val bc = branches.nextSample

      // Continue exploring the search tree to depth `depth`.
      val (sols, brs) = bc(branches.totalWeight, depth)
      solutions ++= sols
      branches = brs
    }
    solutions
  }

  /**
   * Locally explore the search tree defined by a random variable.
   *
   * @returns a pair `(solutions, branches)` with `solutions` a
   *          distribution over solutions discovered thus far, and
   *          `branches` a distribution over unexplored branches.
   */
  def explore[A, B](x: RandVar[A], p: Prob, env: Environment, depth: Int)(
    cont: (A, Prob, Environment, Int) => ExploreRes[B])
      : ExploreRes[B] = x match {
    case x @ RandVarChoice(d) => x.choice(env) match {
      case Some(v) => {
        assert(d exists (_._1 == v), v + " not in " + d)
        cont(v, p, env, depth)
      }
      case None => {
        // Closure for further exploring this branch
        val exploreChoices = new BranchClosure[B] {
          def apply(p, k) = {
            val res = d map {
              case (v, q) =>
                x.withChoice(env, v) { e =>
                  cont(v, p * q, e, k)
                }
            }
            val z: ExploreRes[B] = (dist(), dist())
            res.foldLeft(z){
              case ((sAcc, bAcc), (s, b)) => (sAcc ++ s, bAcc ++ b)
            }
          }
        }

        // Don't count certainties as choices.
        val depth1 =
          if (!d.isEmpty && d.tail.isEmpty) depth else depth - 1
        if (depth1 < 0) {
          // Return parameters needed to further explore this branch
          (dist(), dist(exploreChoices -> p))
        } else {
          // Explore sub-branches and return result
          exploreChoices(p, depth1)
        }
      }
    }
    case t @ RandVarFlatMap(x, f) => explore(x, p, env, depth){ (y, q, e, k) =>
      t.choice(env) match {
        case Some(r) => explore(r, q, e, k)(cont)
        case None => {
          val r = f(y)
          t.withChoice(e, r) { e1 =>
            explore(r, q, e1, k)(cont)
          }
        }
      }
    }
    case RandVarOrElse(x, y) => {
      val (xd, xe) = explore(x, p, env, depth)(cont)
      val (yd, ye) = explore(y, p, env, depth)(cont)
      (xd ++ yd, xe ++ ye)
    }
  }
}

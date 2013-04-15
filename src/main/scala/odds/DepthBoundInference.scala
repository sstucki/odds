package odds

/**
 * Depth-bound inference for the ODDS language.
 *
 * This trait provides a depth-bound inference algorithm based on a
 * depth-first, iterative-deepening traversal of the search tree
 * defined by a probabilistic computation.
 */
trait DepthBoundInference extends OddsIntf with DistIterables with CommittedChoices {

  type Rand[+A] = RandVar[A]

  sealed abstract class RandVar[+A] extends RandIntf[A] {

    def flatMap[B](f: A => Rand[B]): Rand[B] = RandVarFlatMap(this, f)

    def orElse[B >: A](that: Rand[B]): Rand[B] = RandVarOrElse(this, that)

    def reify0[B](p: Prob, depth: Int)(
      cont: (A, Prob, Int) => (Dist[B], Prob)): (Dist[B], Prob)

    /**
     * Reify a random variable representing a probabilistic computation.
     *
     * This method explores the search tree associated with the
     * probabilistic computation represented by this random variable
     * until either
     *
     *  1. the number of explored values of the distribution is
     *     greater than `solutions`, or
     *
     *  2. the upper bound on the total weight of unexplored values is
     *     lower than `error`.
     *
     * @param solutions the minimum number of explored values in the
     *        resulting distribution.
     * @param error the maximum upper bound on the total weight of
     *        unexplored values.
     * @return a pair `(d, w)`, with `d` the distribution over the
     *         explored values of this random variable, and `w` the
     *         sum of the weight of the unexplored values of this
     *         random variable.
     */
    def reify(solutions: Int, error: Prob = 0.0): (Dist[A], Prob) = {
      var dist: Dist[A] = Iterable()
      var err: Prob = error + 1
      var depth: Int = 1
      while ((dist.size < solutions) && (err > error)) {
        //print("depth " + depth + "... ")
        val (dist1, err1) = reify0(1, depth){ (x, p, k) => (Iterable(x -> p), 0.0) }
        dist = dist1
        err = err1
        //println("err = " + err + ", dist = " + dist)
        depth += 1
      }
      (consolidate(dist), err)
    }
  }

  final case class RandVarChoice[+A](dist: Dist[A])
      extends RandVar[A]
      with CommittedChoice[A] {

    def reify0[B](p: Prob, depth: Int)(
      cont: (A, Prob, Int) => (Dist[B], Prob)): (Dist[B], Prob) = choice match {
        case Some(v) => {
          assert(dist exists (_._1 == v), v + " not in " + dist)
          cont(v, p, depth)
        }
        case None => {
          // Don't count certainties as choices.
          val depth1 =
            if (!dist.isEmpty && dist.tail.isEmpty) depth else depth - 1
          if (depth1 < 0) (Iterable(), p) else {
            val res = dist map {
              case (v, q) =>
                commit(v)
                cont(v, p * q, depth1)
            }
            relax
            res.foldLeft((Iterable[(B, Prob)](), 0.0)){
              case ((dAcc, eAcc), (d, e)) => (dAcc ++ d, eAcc + e)
            }
          }
        }
      }
  }

  final case class RandVarFlatMap[+A, B](x: RandVar[B], f: B => Rand[A])
    extends RandVar[A] with CommittedChoice[Rand[A]] {

    def reify0[T](p: Prob, depth: Int)(
      cont: (A, Prob, Int) => (Dist[T], Prob)): (Dist[T], Prob) =
      x.reify0(p, depth){ (y, q, k) =>
        choice match {
          case Some(r) => r.reify0(q, k)(cont)
          case None => {
            val r = f(y)
            commit(r)
            val d = r.reify0(q, k)(cont)
            relax
            d
          }
        }
      }
  }

  final case class RandVarOrElse[+A](x: RandVar[A], y: RandVar[A])
      extends RandVar[A] {

    def reify0[B](p: Prob, depth: Int)(
      cont: (A, Prob, Int) => (Dist[B], Prob)): (Dist[B], Prob) = {
      val (xd, xe) = x.reify0(p, depth)(cont)
      val (yd, ye) = y.reify0(p, depth)(cont)
      (xd ++ yd, xe + ye)
    }
  }


  def choice[A](xs: (A, Prob)*): Rand[A] = RandVarChoice(xs)
}

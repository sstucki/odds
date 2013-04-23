package odds

/**
 * Depth-bound inference for the ODDS language.
 *
 * This trait provides a depth-bound inference algorithm based on a
 * depth-first, iterative-deepening traversal of the search tree
 * defined by a probabilistic computation.
 */
trait DepthBoundInference extends DelayedChoiceIntf with DistMaps {

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
   * @param x the random variable to reify.
   */
  def reify[A](solutions: Int, error: Prob = 0.0)(
    x: Rand[A]): (Dist[A], Prob) = {

    var d: Dist[A] = dist()
    var err: Prob = error + 1
    var depth: Int = 1
    while ((d.size < solutions) && (err > error)) {
      print("depth " + depth + "... ")
      val (d1, err1) = explore(x, 1, Map(), depth) {
        (x, p, e, k) => (dist(x -> p), 0.0)
      }
      d = consolidate(d1)
      err = err1
      println("err = " + err + " (thsld = " + error + "), d.size = " + d.size)
      depth += 1
    }
    (d, err)
  }

  /**
   * Explore the search tree of a random computation to a limited
   * depth using continuation-passing style.
   */
  def explore[A, B](x: Rand[A], p: Prob, env: Environment, depth: Int)(
    cont: (A, Prob, Environment, Int) => (Dist[B], Prob)): (Dist[B], Prob) =
    x match {
      case x @ RandVarChoice(d) => x.choice(env) match {
        case Some(v) => {
          assert(d exists (_._1 == v), v + " not in " + d)
          cont(v, p, env, depth)
        }
        case None => {
          // Don't count certainties as choices.
          val depth1 =
            if (!d.isEmpty && d.tail.isEmpty) depth else depth - 1
          if (depth1 < 0) (dist(), p) else {
            val res = d map {
              case (v, q) =>
                x.withChoice(env, v) { e =>
                  cont(v, p * q, e, depth1)
                }
            }
            ((dist(): Dist[B], 0.0) /: res) {
              case ((dAcc, eAcc), (d, e)) => (dAcc ++ d, eAcc + e)
            }
          }
        }
      }
      case t @ RandVarFlatMap(x, f) => explore(x, p, env, depth) {
        (y, q, e, k) => t.choice(env) match {
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
        (xd ++ yd, xe + ye)
      }
    }
}

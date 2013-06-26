package odds

/** Basic tree generator interface. */
trait TreeGenIntf {
  this: OddsIntf with DistIntf =>

  /** Tree class. */
  sealed abstract class Tree[+A] {

    /** Remove "singleton choices". */
    def compact: Tree[A] = this match {
      case TreeNode(Dist((t, 1.0))) => t.compact
      case TreeNode(d) => {
        val di = d map { case (t, p) => (t.compact, p) }
        TreeNode(Dist(di.toSeq: _*))
      }
      case t => t
    }

    /** Generate source code for a TikZ tree diagram. */
    def mkTikzString: String = {

      val formatter = new java.text.DecimalFormat("#.####")
      def fmt(p: Prob) = formatter format p

      def nodeStyle(t: Tree[A]) = t match {
        case TreeNode(d) => if (d.isEmpty) "fail" else "intl"
        case TreeLeaf(_) => "leaf"
      }

      def mkTikzString0(t: Tree[A], i: Int, p: Prob, pTot: Prob,
        root: Boolean, head: Boolean): String = {
        val is = " " * i
        var st = is + "node[" + nodeStyle(t) + "] "
        t match {
          case TreeNode(d) => {
            var h = true
            st += "{}"
            for ((t, q) <- d) {
              st += "\n" + is + "child[" + nodeStyle(t) + "Child] {\n"
              st += mkTikzString0(t, i + 2, q, pTot * q, false, h)
              st += is + "}"
              h = false
            }
          }
          case TreeLeaf(v) => {
            st += "{$" + v + "$\\\\" + fmt(pTot) + "}"
          }
        }
        if (root) {
          st += ";"
        } else {
          st += "\n" + is + "edge from parent[" + nodeStyle(t) + "Edge]"
          st += " node[above " + (if (head) "left" else "right")
          st += "] {" + fmt(p) + "}\n"
        }
        st
      }
      "\\" + mkTikzString0(this.compact, 0, 1.0, 1.0, true, false)
    }
  }

  final case class TreeNode[+A](children: Dist[Tree[A]]) extends Tree[A]
  final case class TreeLeaf[+A](v: A) extends Tree[A]
}


/** Generate search tree for "classic" probability monad. */
trait ProbMonadTreeGen extends TreeGenIntf with OddsIntf
    with DistIterables {

  /** Use search trees directly as the probability monad. */
  type Rand[+A] = RandTree[A]

  /** Internal tree nodes. */
  final case class RandTree[+A](n: TreeNode[A]) extends RandIntf[A] {

    /** Monadic *bind* operation. */
    def flatMap[B](f: A => RandTree[B]): RandTree[B] = {

      def dft(t: Tree[A], f: A => RandTree[B]): TreeNode[B] =
        t match {
          case TreeNode(Dist((t, 1.0))) => dft(t, f)
          case TreeNode(d) => TreeNode(
            d map { case (t, q) => (dft(t, f), q) })
          case TreeLeaf(v) => f(v).n
        }

      RandTree(dft(this.n, f))
    }

    /** Monadic *plus* operation. */
    def orElse[B >: A](that: RandTree[B]): RandTree[B] =
      RandTree(TreeNode(this.n.children ++ that.n.children))
  }

  /** Create a "flat" tree over a distribution. */
  def choice[A](xs: (A, Prob)*): Rand[A] = RandTree(TreeNode(Dist(
    xs.map{ case (v, p) => (TreeLeaf(v), p) }: _*)))

  /**
   * Reify a random variable representing a probabilistic computation
   * into a search tree (no-op).
   *
   * @return the search tree corresponding to this random variable.
   */
  def reify[A](x: Rand[A]): Tree[A] = x.n
}

/** Generate search tree for delayed choice monad. */
trait DelayedChoiceTreeGen extends TreeGenIntf with DelayedChoiceIntf {

  /**
   * Reify a random variable representing a probabilistic computation
   * into a search tree.
   *
   * @return the search tree corresponding to this random variable.
   */
  def reify[A](x: Rand[A]): Tree[A] = {

    // depth-first traversal of search tree
    def dft(r: ExploreRes[A]): Tree[A] = {
      val ExploreRes(sls, bts) = r

      val leaves = sls map { case (v, q) => (TreeLeaf(v), q) }
      val nodes = bts map { case (t, q) => (dft(t(1.0)), q) }
      val children = leaves ++ nodes
      TreeNode(children)
    }

    dft(explore(x))
  }
}

package odds
package inference

/** Basic tree generator interface. */
trait TreeGenIntf {
  this: OddsIntf =>

  /** Tree class. */
  sealed abstract class Tree[+A] {

    /** Remove "singleton choices". */
    def compact: Tree[A] = this match {
      case TreeNode(Dist((t, 1.0)), _) => t.compact
      case TreeNode(d, n) => {
        val di = d map { case (t, p) => (t.compact, p) }
        TreeNode(Dist(di.toSeq: _*), n)
      }
      case t => t
    }

    /** Generate source code for a TikZ tree diagram. */
    def mkTikzString: String = {

      val formatter = new java.text.DecimalFormat("#.####")
      def fmt(p: Prob) = formatter format p

      def nodeStyle(t: Tree[A]) = t match {
        case TreeNode(d, _) => if (d.isEmpty) "fail" else "intl"
        case TreeLeaf(_)    => "leaf"
        case TreeStub(_)    => "stub"
      }

      def nodeName(t: Tree[A]) = t match {
        case TreeNode(_, n) => "={" + n + "}"
        case TreeLeaf(_)    => ""
        case TreeStub(n)    => "={" + n + "}"
      }

      def mkTikzString0(t: Tree[A], i: Int, p: Prob, pTot: Prob,
        root: Boolean, head: Boolean): String = {
        val is = " " * i
        var st = is + "node[" + nodeStyle(t) + nodeName(t) + "] "
        t match {
          case TreeNode(d, _) => {
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
          case TreeStub(_) => st += "{}"
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
      "\\" + mkTikzString0(this, 0, 1.0, 1.0, true, false)
    }
  }

  final case class TreeNode[+A](
    children: Dist[Tree[A]], name: String) extends Tree[A]
  final case class TreeLeaf[+A](v: A) extends Tree[A]
  final case class TreeStub(name: String) extends Tree[Nothing]
}


/** Generate search tree for delayed choice monad. */
trait DelayedChoiceTreeGen extends TreeGenIntf with DelayedChoiceIntf {
  this: OddsLang =>

  /** Concrete probability monad type class. */
  implicit object Rand extends DelayedChoiceMonad

  /**
   * Reify a random variable representing a probabilistic computation
   * into a search tree.
   *
   * @return the search tree corresponding to this random variable.
   */
  def reify[A](x: Rand[A], maxDepth: Option[Int] = None): Tree[A] = {

    // Generate new IDs top-down for every generated tree
    var idCounter = 0
    val newIdsMap = collection.mutable.Map[CommittedChoice.Id, String]()
    def lookUpId(id: CommittedChoice.Id) = newIdsMap.getOrElse(id, {
      idCounter += 1
      val n = idCounter.toString
      newIdsMap += ((id, n))
      n
    })

    // depth-first traversal of search tree
    def dft(r: ExploreRes[A], d: Option[Int]): Tree[A] = {
      val ExploreRes(sls, bts, i) = r
      val n = lookUpId(i)
      if (d.getOrElse(1) <= 0) TreeStub(n)
      else {
        val d1 = d.map(_ - 1)
        val leaves = sls map { case (v, q) => (TreeLeaf(v), q) }
        val nodes = bts map { case (t, q) => (dft(t(1.0), d1), q) }
        val children = leaves ++ nodes
        TreeNode(children, n)
      }
    }

    println("generating tree...")
    dft(explore(x), maxDepth)
  }
}

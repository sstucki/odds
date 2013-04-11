package odds

/** Simple, exact inference in the ODDS language. */
trait OddsCore extends OddsIntf with DistCore with CommittedChoices {
  this: DistIntf =>

  type Rand[+A] = RandVar[A]

  final case class RandVar[+A](dist: Dist[A])
      extends RandIntf[A]
      with CommittedChoice[A] {

    // FIXME 1: Need synchronization in concurrent contexts...
    // FIXME 2: Should we consolidate the resulting distribution here?
    def flatMap[B](f: A => Rand[B]): Rand[B] = choice match {
      case Some(v) => f(v)
      case None => {
        val d = dist flatMap { case (v, p) =>
          commit(v)
          scale(p, f(v).dist)
        }
        relax
        RandVar(d)
      }
    }

    // FIXME: Should we consolidate the resulting distribution here?
    def orElse[B >: A](that: Rand[B]): Rand[B] =
      RandVar(dist ++ that.dist)

    def reify: Dist[A] = consolidate(dist)
  }

  def choice[A](xs: (A, Prob)*): Rand[A] = RandVar[A](xs)
}

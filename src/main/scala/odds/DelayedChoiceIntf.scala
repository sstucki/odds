package odds

import scala.collection.mutable

/**
 * Odds interface with delayed-choice probability monad.
 *
 * This trait defines the [[Rand]] probability monad use in all
 * inference algorithms based on delayed evaluation.
 */
trait DelayedChoiceIntf extends OddsIntf {
  this: DistIntf =>

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
}

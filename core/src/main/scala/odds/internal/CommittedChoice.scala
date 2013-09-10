package ch.epfl.lamp.odds
package internal

/** A trait representing commitments to choices as state. */
trait CommittedChoice[+A] {

  import CommittedChoice._

  /** The unique ID of this choice. */
  val id = freshId

  /** Find a particular choice this random variable might be committed to. */
  @inline final def choice(env: Environment): Option[A] =
    env.get(id).map(_.asInstanceOf[A])

  /** Commit to a given choice of value. */
  @inline final def commit[B >: A](env: Environment, v: B): Environment = {
    env.updated(id, v)
  }

  /** Renounce the commitment to a specific value. */
  @inline final def relax(env: Environment): Environment = {
    env - id
  }

  /** Execute a closure with a given commitment. */
  @inline final def withChoice[B >: A, C](env: Environment, v: B)(
    f: Environment => C): C = {
    val env1 = commit(env, v)
    val r = f(env1)
    r
  }
}

/** Companion object of the [[CommittedChoice]] class. */
object CommittedChoice {

  /**
   * Identifier class.
   *
   * The body of this class is intentionally left empty.  Checking two
   * instances of this class for equality will return `false` unless
   * they refer to the same object (reference equality).  Hence we can
   * use instances of this class as unique identifiers.  The advantage
   * of using this class over e.g. a counter, is that we don't have to
   * explicitly synchronize `freshId` to guarantee thread safety (the
   * synchronization is handled by the run-time system).
   */
  final class Id

  type Environment = scala.collection.immutable.Map[Id, Any];

  /** Generate a fresh ID for a [[CommittedChoice]] instances. */
  def freshId: Id = new Id
}

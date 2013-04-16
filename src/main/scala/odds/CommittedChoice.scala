package odds

/** A trait representing commitments to choices as state. */
trait CommittedChoice[+A] {

  import CommittedChoice._

  /** The unique ID of this choice. */
  val id = freshId

  /** Find a particular choice this random variable might be committed to. */
  def choice(env: Environment): Option[A] =
    env.get(id).map(_.asInstanceOf[A])

  /** Commit to a given choice of value. */
  def commit[B >: A](env: Environment, v: B): Environment = {
    env.updated(id, v)
  }

  /** Renounce the commitment to a specific value. */
  def relax(env: Environment): Environment = {
    env - id
  }

  /** Execute a closure with a given commitment. */
  def withChoice[B >: A, C](env: Environment, v: B)(f: Environment => C): C = {
    val env1 = commit(env, v)
    val r = f(env1)
    r
  }
}

/** Companion object of the [[CommittedChoice]] class. */
object CommittedChoice {

  type Environment = scala.collection.immutable.Map[Int, Any];

  /** Counter to generate new IDs for [[CommittedChoice]] instances. */
  var idCounter = 0

  /** Generate a fresh ID for a [[CommittedChoice]] instances. */
  def freshId: Int = {
    val id = idCounter
    idCounter += 1
    id
  }
}

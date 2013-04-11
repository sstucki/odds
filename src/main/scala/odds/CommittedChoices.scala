package odds

/** Container trait for the CommittedChoice trait. */
trait CommittedChoices {

  /** A trait representing commitments to choices as state. */
  trait CommittedChoice[+A] {

    /** A particular choice this random variable might be committed to. */
    protected[this] var choice: Option[A] = None

    /** Commit to a given choice of value. */
    protected[this] def commit(v: A) {
      choice = Some(v)
    }

    /** Renounce the commitment to a specific value. */
    protected[this] def relax {
      choice = None
    }
  }
}

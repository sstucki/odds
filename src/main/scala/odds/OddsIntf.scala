package odds

/** ODDS Language interface. */
trait OddsIntf {
  this: DistIntf =>

  /**
   * Probability weight type.
   *
   * Note: this need not be normalized to lie within `[0, 1]`.
   */
  type Prob = Double

  /** Random variable monad. */
  type Rand[+A] <: RandIntf[A]

  /**
   * Random variable monad interface.
   *
   * @tparam A the type of the support of this random variable.
   */
  trait RandIntf[+A] {

    // What is this?
    var name = super.toString
    override def toString = name
    def dbg(n:String): this.type = { name = n; this }

    /**
     * Reify a random variable representing a probabilistic computation.
     *
     * @return the distribution over the values of this random variable.
     */
    def reify: Dist[A]


    // -- Zero/plus monad API --

    /** Monadic *bind* operation. */
    def flatMap[B](f: A => Rand[B]): Rand[B]

    /** Monadic *plus* operation. */
    def orElse[B >: A](that: Rand[B]): Rand[B]

    def map[B](f: A => B): Rand[B] = flatMap(x => always(f(x)))
  }

  /**
   * Make a probabilistic choice.
   *
   * @tparam A the type of the support of the random variable
   *         returned by this function.
   * @param xs A sequence of value-weight pairs representing the
   *        discrete distribution to chose from.
   * @return a random variable representing the outcome of the choice.
   */
  def choice[A](xs: (A, Prob)*): Rand[A]

  def always[A](x: A) = choice(x -> 1.0)
  def never = choice()
  def flip(p: Double): Rand[Boolean] = choice(true -> p, false -> (1-p))
  def uniform[A](xs: A*): Rand[A] = choice(xs.map((_, 1.0)):_*)

  //def collapse2[A](r: Rand[A], strategy: String, solutions: Int): List[(A,Prob)]
}

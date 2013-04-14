package odds

/** ODDS Language interface. */
trait OddsIntf {

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
    this: Rand[A] =>

    // What is this?
    var name = super.toString
    override def toString = name
    def dbg(n:String): this.type = { name = n; this }


    // -- Observations/conditional probabilities

    /**
     * Condition this random variable on an observation.
     *
     * Use case:
     *
     * {{{
     *     val coin1 = flip(0.5)
     *     val coin2 = flip(0.5)
     *     val both  = coin1 && coin2
     *
     *     // conditional probability: P(both | coin1) = P(coin2)
     *     both when coin1
     * }}}
     *
     * @param that the observation (random variable) to condition on.
     * @returns the conditional probability of `this` given `that`,
     *          i.e. `P(this | that)`
     */
    def when(that: Rand[Boolean]): Rand[A] = that flatMap {
      case true  => this
      case false => never
    }


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
  def uniform[A](xs: A*): Rand[A] = if (xs.isEmpty) never else {
    val p = 1.0 / xs.size
    choice(xs.map((_, p)):_*)
  }
}

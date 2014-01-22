package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import inference._

class CoinModelTest
    extends OddsLang
    with ExactInference
    with OddsPrettyPrint
    with FlatSpecLike
    with Matchers {

  import Rand.ToScalaMonadic

  behavior of "ExactInference"

  it should "show the results of cond1" in {
    val cond1 = reify {
      val x = flip(0.5)
      x flatMap {
        case true => always(1)
        case _ => x map  {
          case true => 2
          case _ => 3
        }
      }
    }
    show(cond1, "cond1")
    cond1.toMap should equal (Map(1 -> 0.5, 3 -> 0.5))
  }

  it should "show the results of cond2" in {
    val cond2 = reify {
      val x = flip(0.5)
      val y = flip(0.5)
      val both  = x && y

      // conditional probability: P(both | x) = P(y)
      both when x
    }
    show(cond2, "cond2")
    cond2.toMap should equal (Map(true -> 0.25, false -> 0.25))
  }

  it should "show the results of cond3" in {
    val cond3 = reify {
      val x = flip(0.25)
      x flatMap {
        case true  => always(1)
        case false => uniform(2, 3, 4)
      }
    }
    show(cond3, "cond3")
    cond3.toMap should equal (Map(1 -> 0.25, 2 -> 0.25, 3 -> 0.25, 4 -> 0.25))
  }

  it should "show the results of coinModel1a" in {
    val coinModel1a = reify(for {
      coin <- choice(0 -> 0.5, 1 -> 0.5)
    } yield {
      val sum1 = coin + coin
      val sum2 = sum1 + coin
      sum2
    })
    show(coinModel1a, "coinModel1a")
    coinModel1a.toMap should equal (Map(0 -> 0.5, 3 -> 0.5))
  }

  it should "show the results of coinModel1b" in {
    val coinModel1b = reify {
      val coin = choice(0 -> 0.5, 1 -> 0.5)
      val sum1 = coin + coin
      val sum2 = sum1 + coin
      sum2
    }
    show(coinModel1b, "coinModel1b")
    coinModel1b.toMap should equal (Map(0 -> 0.5, 3 -> 0.5))
  }

  it should "show the results of coinModel2a" in {
    val coinModel2a = reify(for {
      coin <- choice(0 -> 0.5, 1 -> 0.5)
    } yield {
      val sum1 = coin + coin
      val sum2 = sum1 + coin
      if (sum2 == 3) sum1 else coin
    })
    show(coinModel2a, "coinModel2a")
    coinModel2a.toMap should equal (Map(0 -> 0.5, 2 -> 0.5))
  }

  it should "show the results of coinModel2b" in {
    val coinModel2b = reify {
      val coin = choice(0 -> 0.5, 1 -> 0.5)
      val sum1 = coin + coin
      val sum2 = sum1 + coin
      (sum2 == always(3)) flatMap {
        case true => sum1
        case false => coin
      }
    }
    show(coinModel2b, "coinModel2b")
    coinModel2b.toMap should equal (Map(0 -> 0.5, 2 -> 0.5))
  }

  it should "show the results of coinModel3a" in {
    val coinModel3a = reify {
      def regularCoin = flip(0.5)
      val c = regularCoin
      for (c1 <- c; c2 <- c) yield (c1, c2)
    }
    show(coinModel3a, "coinModel3a")
    coinModel3a.toMap should equal (
      Map((true, true) -> 0.5, (false, false) -> 0.5))
  }

  it should "show the results of coinModel3b" in {
    val coinModel3b = reify {
      def magicCoin = always(true).flatMap {
        case true => flip(0.5)
        case _    => never
      }
      val c = magicCoin
      for (c1 <- c; c2 <- c) yield (c1, c2)
    }
    show(coinModel3b, "coinModel3b")
    coinModel3b.toMap should equal (
      Map((true, true) -> 0.5, (false, false) -> 0.5))
  }

  it should "show the results of flipping 20 coins" in {

    // Flip `n` coins.
    def flips(p: Prob, n: Int): List[Rand[Boolean]] = n match {
      case 0 => Nil
      case n => flip(p) :: flips(p, n - 1)
    }

    // Check whether a list of coins are all `true`.
    def trues(cs: List[Rand[Boolean]]): Rand[Boolean] = cs match {
      case Nil     => always(true)
      case c :: cs => c && trues(cs)
    }

    val flips20 = reify(trues(flips(0.5, 20)))
    show(flips20, "20 flips")
    val exact = 1.0 / (1 << 20)
    flips20.toMap should equal (
      Map(true -> exact, false -> (1.0 - exact)))

    // Map for counting actual `&&` events.
    val events = scala.collection.mutable.HashMap[Boolean, Int]() ++=
      Map(true -> 0, false -> 0)

    // Check whether a list of coins are all `true` and record events.
    def truesCount(cs: List[Rand[Boolean]]): Rand[Boolean] = cs match {
      case Nil     => always(true)
      case c :: cs => c flatMap { c =>
        events(c) += 1 // Count event
        if (c) truesCount(cs) else always(false)
      }
    }

    val flips20count = reify(truesCount(flips(0.5, 20)))
    show(flips20count, "20 flips (when counting events)")
    println("events: " + events)
    flips20count should equal (flips20)
    events.toMap should equal (Map(true -> 20, false -> 20))
  }

}

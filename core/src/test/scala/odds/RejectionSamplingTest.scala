package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import inference._

class RejectionSamplingTest
    extends OddsLang
    with RejectionSampling
    with OddsPrettyPrint
    with FlatSpecLike
    with Matchers {

  behavior of "OddsExact"

  it should "show the results of condCoins" in {
    val condCoins = sample(1000){
      val x = flip(0.5)
      val y = flip(0.5)
      val both  = x && y

      // conditional probability: P(both | x) = P(y)
      both when x
    }
    show(condCoins, "condCoins")
    condCoins should have size (2)
    condCoins foreach {
      case (v, p) =>
        p should be (0.250 plusOrMinus 0.125)
    }
  }

  it should "show the results of cond3" in {
    import Rand.ToScalaMonadic
    val splitTrials = sample(1000){
      val x = flip(0.25)
      x flatMap {
        case true  => always(1)
        case false => uniform(2, 3, 4)
      }
    }
    show(splitTrials, "splitTrials")
    splitTrials should have size (4)
    splitTrials foreach {
      case (v, p) =>
        p should be (0.250 plusOrMinus 0.125)
    }
  }

  it should "show the results of coinModel3b" in {
    import Rand.ToScalaMonadic
    val magicCoins = sample(1000) {
      def magicCoin = always(true).flatMap {
        case true => flip(0.5)
        case _    => never
      }
      val c = magicCoin
      for (c1 <- c; c2 <- c) yield (c1, c2)
    }
    show(magicCoins, "magicCoins")
    magicCoins should have size (2)
    magicCoins foreach {
      case (v, p) =>
        p should be (0.500 plusOrMinus 250)
    }
  }
}

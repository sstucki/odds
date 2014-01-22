package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import inference._

trait DrunkCoinModel extends OddsLang {

  import Rand.ToScalaMonadic

  def drunkCoin: Rand[Boolean] = {
    val toss = flip(0.5)
    val lost = flip(0.9)
    val x = toss <= toss
    val y = toss ^ true
    lost flatMap {
      case true  => never
      case false => toss
    }
  }

  def dcoinAnd(n: Int): Rand[Boolean] = n match {
    case 1 => drunkCoin
    case n => drunkCoin && dcoinAnd(n - 1)
  }
}


class DrunkCoinModelTest
    extends FlatSpecLike
    with Matchers {

  behavior of "LocalImportanceSampling"

  it should "perform exact inference of drunkCoin" in {
    new DrunkCoinModel with ExactInference with OddsPrettyPrint {
      val d = reify(dcoinAnd(10))
      show(d, "dcointAnd(10), exact")
      d foreach {
        case (false, p) => p should be (5.263157894e-2  plusOrMinus 1e-11)
        case (true, p) =>  p should be (9.765624999e-14 plusOrMinus 1e-23)
      }
    }
  }

  it should "perform local importance sampling for drunkCoin" in {
    new DrunkCoinModel with LocalImportanceSampling with OddsPrettyPrint {
      val d = sample(5000, 4)(dcoinAnd(10))
      show(d, "dcoinAnd(10), local importance sampling")
      d foreach {
        case (false, p) => p should be (5.263157894e-2  plusOrMinus 1e-4)
        case (true, p) =>  p should be (9.765624999e-14 plusOrMinus 1e-13)
      }
    }
  }

  it should "perform local rejection sampling for drunkCoin" in {
    new DrunkCoinModel with RejectionSampling with OddsPrettyPrint {
      val d = sample(10000)(dcoinAnd(10))
      show(d, "dcoinAnd(10), rejection sampling")
      d foreach {
        case (false, p) => p should be > 0.03
        case (true, p) =>  p should be < 1e-13
      }
    }
  }
}

package odds

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

trait DrunkCoinModel extends OddsLang {

  def drunkCoin: Rand[Boolean] = {
    val toss = flip(0.5)
    val lost = flip(0.9)
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
    extends FlatSpec
    with ShouldMatchers {

  behavior of "LocalImportanceSampling"

  it should "perform exact inference of drunkCoin" in {
    new DrunkCoinModel with ExactInference with OddsPrettyPrint {
      val d = dcoinAnd(10).reify
      show(d, "dcointAnd(10), exact")
      d foreach {
        case (false, p) => p should be (0.052631 plusOrMinus 1e-6)
        case (true, p) =>  p should be (9.7656e-14 plusOrMinus 1e-16)
      }
    }
  }

  it should "perform local importance sampling for drunkCoin" in {
    new DrunkCoinModel with LocalImportanceSampling with OddsPrettyPrint {
      val d = sample(5000, 5)(dcoinAnd(10))
      show(d, "dcoinAnd(10), local importance sampling")
      normalize(d) foreach {
        case (false, p) => p should be (0.999999999 plusOrMinus 1e-9)
        case (true, p) =>  p should be (1.85546e-12 plusOrMinus 1e-11)
      }
    }
  }

  it should "perform local rejection sampling for drunkCoin" in {
    new DrunkCoinModel with RejectionSampling with OddsPrettyPrint {
      val d = sample(10000)(dcoinAnd(10))
      show(d, "dcoinAnd(10), rejection sampling")
      d foreach {
        case (false, p) => p should be > 200.0
        case (true, p) =>  p should be < 5.0
      }
    }
  }
}

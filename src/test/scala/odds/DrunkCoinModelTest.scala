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
      d should equal (Map(List(true, true, true, false, false) -> 1.0))
    }
  }

  it should "perform local importance sampling for drunkCoin" in {
    new DrunkCoinModel with LocalImportanceSampling with OddsPrettyPrint {
      val d = sample(5000, 3)(dcoinAnd(10))
      show(d, "dcoinAnd(10), local importance sampling")
      d should equal (Map(List(true, true, true, false, false) -> 1.0))
    }
  }

  it should "perform local rejection sampling for drunkCoin" in {
    new DrunkCoinModel with RejectionSampling with OddsPrettyPrint {
      val d = sample(5000)(dcoinAnd(10))
      show(d, "dcoinAnd(10), rejection sampling")
      d should equal (Map(List(true, true, true, false, false) -> 1.0))
    }
  }
}

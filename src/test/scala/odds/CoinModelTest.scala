package odds

import org.scalatest.FlatSpec

class CoinModelTest
    extends OddsLang
    with OddsExact
    with OddsPrettyPrint
    with FlatSpec {

  // FIXME: Actually compare against expected results.

  behavior of "OddsExact"

  it should "show the results of cond1" in {
    val cond1 = {
      val x = flip(0.5)
      x flatMap {
        case true => always(1)
        case _ => x map  {
          case true => 2
          case _ => 3
        }
      }
    }.reify
    show(cond1, "cond1")
    expectResult(Map(1 -> 0.5, 3 -> 0.5))(cond1)
  }

  it should "show the results of cond2" in {
    val cond2 = {
      val x = flip(0.5)
      val y = flip(0.5)
      val both  = x && y

      // conditional probability: P(both | x) = P(y)
      both when x
    }.reify
    show(cond2, "cond2")
    expectResult(Map(true -> 0.25, false -> 0.25))(cond2)
  }

  it should "show the results of coinModel1a" in {
    val coinModel1a = (for {
      coin <- choice(0 -> 0.5, 1 -> 0.5)
    } yield {
      val sum1 = coin + coin
      val sum2 = sum1 + coin
      sum2
    }).reify
    show(coinModel1a, "coinModel1a")
    expectResult(Map(0 -> 0.5, 3 -> 0.5))(coinModel1a)
  }

  it should "show the results of coinModel1b" in {
    val coinModel1b = {
      val coin = choice(0 -> 0.5, 1 -> 0.5)
      val sum1 = coin + coin
      val sum2 = sum1 + coin
      sum2
    }.reify
    show(coinModel1b, "coinModel1b")
    expectResult(Map(0 -> 0.5, 3 -> 0.5))(coinModel1b)
  }

  it should "show the results of coinModel2a" in {
    val coinModel2a = (for {
      coin <- choice(0 -> 0.5, 1 -> 0.5)
    } yield {
      val sum1 = coin + coin
      val sum2 = sum1 + coin
      if (sum2 == 3) sum1 else coin
    }).reify
    show(coinModel2a, "coinModel2a")
    expectResult(Map(0 -> 0.5, 2 -> 0.5))(coinModel2a)
  }

  it should "show the results of coinModel2b" in {
    val coinModel2b = {
      val coin = choice(0 -> 0.5, 1 -> 0.5)
      val sum1 = coin + coin
      val sum2 = sum1 + coin
      (sum2 === always(3)) flatMap {
        case true => sum1
        case false => coin
      }
    }.reify
    show(coinModel2b, "coinModel2b")
    expectResult(Map(0 -> 0.5, 2 -> 0.5))(coinModel2b)
  }
}

package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers
import scala.math._

import inference._

class RandOpsTest
    extends OddsLang
    with RejectionSampling
    with OddsPrettyPrint
    with FlatSpecLike
    with Matchers {
  behavior of "normal"

  it should "have mean 0 and variance 1." in {
    val d = sample(1000)(normal)
    val mean = d.map(_._1).sum / d.size
    mean should be (0.0 +- 0.1)
    val variance = d.map(x => pow(x._1 - mean, 2)).sum / d.size
    variance should be (1.0 +- 0.1)
  }
}

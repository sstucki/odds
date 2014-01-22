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
  behavior of "uniform"

  it should "have mean (upper - lower) / 2 and variance (upper - lower) ^ 2 / 12." in {
    val lower = 1.4
    val upper = 2.7
    val d = sample(1000)(uniform(lower, upper))
    val mean = d.map(_._1).sum / d.size
    mean should be (0.5 * (upper + lower) +- 0.05)
    val variance = d.map(x => pow(x._1 - mean, 2)).sum / d.size
    variance should be (pow(upper - lower, 2) / 12 +- 0.05)
  }
  behavior of "normal"

  it should "have mean 0 and variance 1." in {
    val d = sample(1000)(normal)
    val mean = d.map(_._1).sum / d.size
    mean should be (0.0 +- 0.1)
    val variance = d.map(x => pow(x._1 - mean, 2)).sum / d.size
    variance should be (1.0 +- 0.1)
  }
}

package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import inference._

/**
 * Adopted from an example in "Embedded probabilistic programming" by
 * Oleg Kiselyov and Chung-chieh Shan (in proceedings of the IFIP
 * working conference on domain-specific languages (DSL), ed. Walid
 * Taha. LNCS 5658, Springer, 2009, pp. 360-384), URL:
 * http://okmij.org/ftp/kakuritu/dsl-paper.pdf
 *
 * The original program in Hansei is:
 * {{{
 *   let flip p = dist [(p, true); (1 .- p, false)]
 *
 *   let grass_model () =
 *     let rain = flip 0.3 and sprinkler = flip 0.5 in
 *     let grass_is_wet = flip 0.9 && rain
 *                     || flip 0.8 && sprinkler
 *                     || flip 0.1 in
 *     if grass_is_wet then rain else fail ()
 * }}}
 *
 * See also http://okmij.org/ftp/kakuritu/index.html
 */
trait GrassModel extends OddsLang {

  // This is the standard version of the model.  Some choices are not
  // "uniquely" identified (i.e. they have different IDs in different
  // branches of the search tree) but this doesn't change the outcome.
  def grassModel = {
    val rain       = flip(0.3)
    val sprinkler  = flip(0.5)
    val grassIsWet = {
      flip(0.9) && rain      ||
      flip(0.8) && sprinkler ||
      flip(0.1)
    }
    if (grassIsWet) rain else never
  }

  import Rand.ToScalaMonadic

  // This model neither identifies nor delays choices (i.e. choices
  // have different IDs in every branch and are made exactly in the
  // order specified below).
  def grassModelMonadic = for {
    rain       <- flip(0.3)
    sprinkler  <- flip(0.5)
    x0         <- flip(0.9)
    x1         <- flip(0.8)
    x2         <- flip(0.1)
    grassIsWet =
      x0 && rain      ||
      x1 && sprinkler ||
      x2
    res <- if (grassIsWet) always(rain) else never
  } yield res

  // This model uniquely identifies all choices but does not delay
  // them and it does not perform short-cutting.
  def grassModelIdsNoShortcut = {
    val rain      = flip(0.3)
    val sprinkler = flip(0.5)
    val x0        = flip(0.9)
    val x1        = flip(0.8)
    val x2        = flip(0.1)
    val grassIsWet = for {
      rainv      <- rain
      sprinklerv <- sprinkler
      x0v        <- x0
      x1v        <- x1
      x2v        <- x2
    } yield {
      x0v && rainv      ||
      x1v && sprinklerv ||
      x2v
    }
    if (grassIsWet) rain else never
  }

  // This model uniquely identifies all choices and performs
  // short-cutting, but it does not delay choices.
  def grassModelIdsNoDelay = {
    val rain      = flip(0.3)
    val sprinkler = flip(0.5)
    val x0        = flip(0.9)
    val x1        = flip(0.8)
    val x2        = flip(0.1)
    val grassIsWet = for {
      rainv      <- rain
      sprinklerv <- sprinkler
      res <- {
        x0 && always(rainv)      ||
        x1 && always(sprinklerv) ||
        x2
      }
    } yield res
    if (grassIsWet) rain else never
  }

  // This model uniquely identifies all choices, delays them, and
  // performs short-cutting.
  def grassModelIds = {
    val rain      = flip(0.3)
    val sprinkler = flip(0.5)
    val x0        = flip(0.9)
    val x1        = flip(0.8)
    val x2        = flip(0.1)
    val grassIsWet = {
      x0 && rain      ||
      x1 && sprinkler ||
      x2
    }
    if (grassIsWet) rain else never
  }
}

class GrassModelSpec
    extends GrassModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpecLike
    with Matchers {

  behavior of "GrassModel"

  it should "show the probability of rain given that the grass is wet" in {
    val ds = Array(
      reify(grassModel),
      reify(grassModelMonadic),
      reify(grassModelIdsNoShortcut),
      reify(grassModelIdsNoDelay),
      reify(grassModelIds))
    show(ds(0), "rain (default)")
    show(ds(1), "rain (monadic)")
    show(ds(2), "rain (all IDs unique, no delaying, no shortcuts)")
    show(ds(3), "rain (all IDs unique, no delaying)")
    show(ds(4), "rain (all IDs unique)")
    ds foreach { d =>
      d foreach {
        case (false, p) => p should be (0.322  plusOrMinus 1e-12)
        case (true,  p) => p should be (0.2838 plusOrMinus 1e-12)
      }
    }
  }
}

class GrassModelRejectionSamplingSpec
    extends GrassModel
    with RejectionSampling
    with OddsPrettyPrint
    with FlatSpecLike
    with Matchers {

  behavior of "GrassModel"

  it should "show the result of rejection sampling for the lawn model." in {
    val d = sample(1000)(grassModel)
    show(d, "rain (rejection sampling)")
    d foreach {
      case (false, p) => p should be (0.322  plusOrMinus 1e-1)
      case (true,  p) => p should be (0.2838 plusOrMinus 1e-1)
    }
  }
}

class GrassModelTreeGenSpec
    extends GrassModel
    with DelayedChoiceTreeGen
    with FlatSpecLike
    with Matchers {

  behavior of "DelayedChoiceTreeGen"

  it should "generate the search trees of the grass models" in {
    val ts = Array(
      reify(grassModel),
      reify(grassModelMonadic),
      reify(grassModelIdsNoShortcut),
      reify(grassModelIdsNoDelay),
      reify(grassModelIds))
    val names = Array(
      "default", "monadic", "all IDs unique, no delaying, no shortcuts)",
      "all IDs unique, no delaying", "all IDs unique")
    for (i <- 0 to 4) {
      println("\n\ntree (" + names(i) + "):\n\n" + ts(i).mkTikzString)
    }
    /*
    ts(0) should be (Nil)
    ts(0) should be (Nil)
    ts(0) should be (Nil)
    ts(0) should be (Nil)
    ts(0) should be (Nil)
    */
  }
}

package odds

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

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

  def grassModel = {
    val rain       = flip(0.3)
    val sprinkler  = flip(0.5)
    val grassIsWet =
      flip(0.9) && rain      ||
      flip(0.8) && sprinkler ||
      flip(0.1)
    if (grassIsWet) rain else never
  }
}

trait GrassMonadicModel extends OddsLang {

  def grassModel = for {
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
}

class GrassModelTest
    extends GrassModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpec
    with ShouldMatchers {

  behavior of "GrassModel"

  it should "show the probability of rain given that the grass is wet" in {
    val d = reify(grassModel)
    show(d, "rain")
    d foreach {
      case (false, p) => p should be (0.322  plusOrMinus 1e-12)
      case (true,  p) => p should be (0.2838 plusOrMinus 1e-12)
    }
  }
}

class GrassMonadicModelTest
    extends GrassMonadicModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpec
    with ShouldMatchers {

  behavior of "GrassMonadicModel"

  it should "show the probability of rain given that the grass is wet" in {
    val d = reify(grassModel)
    show(d, "rain")
    d foreach {
      case (false, p) => p should be (0.322  plusOrMinus 1e-12)
      case (true,  p) => p should be (0.2838 plusOrMinus 1e-12)
    }
  }
}

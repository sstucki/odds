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

trait GrassMonadicShortCutModel extends OddsLang {

  def grassModel = for {
    rain       <- flip(0.3)
    sprinkler  <- flip(0.5)
    x0         <- flip(0.9)
    grassIsWet <- if (x0 && rain) always(true) else for {
      x1       <- flip(0.8)
      x2       <- if (x1 && sprinkler) always(true) else flip(0.1)
    } yield x2
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

class GrassMonadicShortCutModelTest
    extends GrassMonadicModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpec
    with ShouldMatchers {

  behavior of "GrassMonadicShortCutModel"

  it should "show the probability of rain given that the grass is wet" in {
    val d = reify(grassModel)
    show(d, "rain")
    d foreach {
      case (false, p) => p should be (0.322  plusOrMinus 1e-12)
      case (true,  p) => p should be (0.2838 plusOrMinus 1e-12)
    }
  }
}

class GrassModelTreeGenTest
    extends GrassModel
    with DelayedChoiceTreeGen
    with FlatSpec
    with ShouldMatchers {

  behavior of "DelayedChoiceTreeGen"

  it should "generate the search tree of GrassModel (delayed choice)" in {
    val t = reify(grassModel)
    println(t.mkTikzString)
    t should be (TreeNode(Dist(
      TreeNode(Dist(
        TreeNode(Dist(
          TreeLeaf(true) -> 1.0)) -> 0.3,
        TreeNode(Dist(TreeNode(Dist(
          TreeNode(Dist(
            TreeLeaf(false) -> 1.0)) -> 0.5,
          TreeNode(Dist(
            TreeNode(Dist(
              TreeLeaf(false) -> 1.0)) -> 0.1,
            TreeNode(Dist()) -> 0.9)) -> 0.5)) -> 0.8,
          TreeNode(Dist(
            TreeNode(Dist(TreeLeaf(false) -> 1.0)) -> 0.1,
            TreeNode(Dist()) -> 0.9)) -> 0.19999999999999996)) -> 0.7)) -> 0.9,
      TreeNode(Dist(
        TreeNode(Dist(
          TreeNode(Dist(
            TreeNode(Dist(
              TreeLeaf(true) -> 1.0)) -> 0.3,
            TreeNode(Dist(
              TreeLeaf(false) -> 1.0)) -> 0.7)) -> 0.5,
          TreeNode(Dist(
            TreeNode(Dist(
              TreeNode(Dist(TreeLeaf(true) -> 1.0)) -> 0.3,
              TreeNode(Dist(TreeLeaf(false) -> 1.0)) -> 0.7)) -> 0.1,
            TreeNode(Dist()) -> 0.9)) -> 0.5)) -> 0.8,
        TreeNode(Dist(
          TreeNode(Dist(
            TreeNode(Dist(TreeLeaf(true) -> 1.0)) -> 0.3,
            TreeNode(Dist(TreeLeaf(false) -> 1.0)) -> 0.7)) -> 0.1,
          TreeNode(Dist()) -> 0.9)) -> 0.19999999999999996)) -> 0.09999999999999998)))
  }
}

class GrassModelProbMonadTreeGenTest
    extends GrassMonadicModel
    with ProbMonadTreeGen
    with FlatSpec
    with ShouldMatchers {

  behavior of "ProbMonadTreeGen"

  it should "generate the search tree of GrassModel (prob monad)" in {
    val t = reify(grassModel)
    println(t.mkTikzString)
    t.compact should be (TreeNode(Dist(
      (TreeNode(Dist(
        (TreeNode(Dist(
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeLeaf(true),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeLeaf(true),0.9))),0.19999999999999996))),0.9),
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeLeaf(true),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.09999999999999998))),0.5),
        (TreeNode(Dist(
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeLeaf(true),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeLeaf(true),0.9))),0.19999999999999996))),0.9),
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeNode(Dist()),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.09999999999999998))),0.5))),0.3),
      (TreeNode(Dist(
        (TreeNode(Dist(
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeLeaf(false),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.9),
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeLeaf(false),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.09999999999999998))),0.5),
        (TreeNode(Dist(
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.9),
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.09999999999999998))),0.5))),0.7))))  }
}

class GrassModelProbMonadShortCutTreeGenTest
    extends GrassMonadicShortCutModel
    with ProbMonadTreeGen
    with FlatSpec
    with ShouldMatchers {

  behavior of "ProbMonadTreeGen"

  it should "generate the search tree of GrassModel (prob monad)" in {
    val t = reify(grassModel)
    println(t.mkTikzString)
    t.compact should be (TreeNode(Dist(
      (TreeNode(Dist(
        (TreeNode(Dist(
          (TreeLeaf(true),0.9),
          (TreeNode(Dist(
            (TreeLeaf(true),0.8),
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.09999999999999998))),0.5),
        (TreeNode(Dist(
          (TreeLeaf(true),0.9),
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeNode(Dist()),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(true),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.09999999999999998))),0.5))),0.3),
      (TreeNode(Dist(
        (TreeNode(Dist(
          (TreeNode(Dist(
            (TreeLeaf(false),0.8),
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.9),
          (TreeNode(Dist(
            (TreeLeaf(false),0.8),
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.09999999999999998))),0.5),
        (TreeNode(Dist(
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.9),
          (TreeNode(Dist(
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.8),
            (TreeNode(Dist(
              (TreeLeaf(false),0.1),
              (TreeNode(Dist()),0.9))),0.19999999999999996))),0.09999999999999998))),0.5))),0.7))))
  }
}

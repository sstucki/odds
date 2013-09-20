package ch.epfl.lamp.odds

import language.implicitConversions

import internal.{ OddsIntf, LogicOps, RandOps }
import macros.{ VirtualAny, VirtualControls }


/** Odds core language API. */
trait OddsLang
    extends OddsIntf
    with VirtualAny
    with VirtualControls
    with LogicOps
    with RandOps {

  import Rand._

  // -- Lifted function applications --
  implicit def liftFun1Fmap[A, B](f: A => B): Rand[A] => Rand[B] =
    fmap(_)(f)
  implicit def liftFun1Bind[A, B](f: A => Rand[B]): Rand[A] => Rand[B] =
    bind(_)(f)

  // -- Lifted tuple/case class construction --
  type RandProduct[P <: Product] = macros.MonadicProduct[Rand, P]
  implicit def prodToRandProd[P <: Product](p: P)(
    implicit rp: RandProduct[P]) = rp.lift(p)

  /**
   * Implicit conversion from lists of random variables to random
   * lists.
   *
   * @todo Can/should we make this more generic?  E.g. implicit
   *   conversions form `Traversable[Rand[A]]` to
   *   `Rand[Traversable[A]]`?
   */
  implicit def listRandToRandList[A](l: List[Rand[A]]): Rand[List[A]] =
    l match {
      case rx :: rxs => bind(rx) { x: A =>
        fmap(listRandToRandList(rxs)) { xs: List[A] => x :: xs }
      }
      case Nil => unit(Nil)
    }
}

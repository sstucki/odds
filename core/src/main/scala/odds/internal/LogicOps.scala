package ch.epfl.lamp.odds
package internal

/**
 * Lifted logic operations.
 *
 * This trait provides lifted versions of some logic operations in the
 * `Rand[_]` monad.
 */
trait LogicOps { this: OddsIntf =>

  import Rand._

  implicit class RandBoolean(rx: Rand[Boolean]) {
    def &&(ry: => Rand[Boolean]): Rand[Boolean] =
      bind(rx) { x => if (x) ry else unit(false) }
    def ||(ry: => Rand[Boolean]): Rand[Boolean] =
      bind(rx) { x => if (x) unit(true) else ry }
  }
}

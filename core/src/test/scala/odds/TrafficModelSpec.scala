package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import inference._

trait TrafficModel extends OddsLang {

  abstract class Light
  case object Red extends Light
  case object Yellow extends Light
  case object Green extends Light

  abstract class Action
  case object Stop extends Action
  case object Drive extends Action

  abstract class Result
  case object Crash extends Result
  case object NoCrash extends Result

  type Driver = Rand[Light] => Rand[Action]

  def trafficLight = choose(Red -> 0.5, Yellow -> 0.1, Green -> 0.4)

  def otherLight(light: Light) = light match {
    case Red => Green
    case Yellow => Red
    case Green => Red
  }

  def otherRandLight(light: Rand[Light]) = Rand(otherLight _)(light)

  def cautiousDriver(light: Light) = light match {
    case Red    => always(Stop)
    case Yellow => choose(Stop -> 0.9, Drive -> 0.1)
    case Green  => always(Drive)
  }

  def aggressiveDriver(light: Light) = light match {
    case Red    => choose(Stop -> 0.9, Drive -> 0.1)
    case Yellow => choose(Stop -> 0.1, Drive -> 0.9)
    case Green  => always(Drive)
  }

  def crash(driver1: Driver, driver2: Driver, light: Rand[Light]) = {
    val d1 = driver1(light)
    val d2 = driver2(otherRandLight(light))
    if ((d1 == Drive) && (d2 == Drive)) {
      choose(Crash -> 0.9, NoCrash -> 0.1)
    } else {
      always(NoCrash)
    }
  }

  def lights = {
    val l1 = trafficLight
    val l2 = otherRandLight(l1)
    (l1, l2)
  }
}

class TrafficModelSpec
    extends TrafficModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpecLike
    with Matchers {

  behavior of "TrafficModel"

  it should "show the combinations of lights" in {
    val d = reify(lights)
    show(d, "lights")
    d foreach {
      case ((Red, Green),  p) => p should be (0.5)
      case ((Green, Red),  p) => p should be (0.4)
      case ((Yellow, Red), p) => p should be (0.1)
    }
  }

  it should "show the results of trafficModel1" in {
    val d = reify(crash(cautiousDriver, aggressiveDriver, trafficLight))
    show(d, "trafficModel1")
    d foreach {
      case (Crash,   p) => p should be (0.0369 plusOrMinus 1e-11)
      case (NoCrash, p) => p should be (0.9631 plusOrMinus 1e-11)
    }
  }

  it should "show the results of trafficModel2" in {
    val d = reify(crash(aggressiveDriver, cautiousDriver, trafficLight))
    show(d, "trafficModel2")
    d foreach {
      case (Crash,   p) => p should be (0.045 plusOrMinus 1e-11)
      case (NoCrash, p) => p should be (0.955 plusOrMinus 1e-11)
    }
  }

  it should "show the results of trafficModel3" in {
    val d = reify(crash(aggressiveDriver, aggressiveDriver, trafficLight))
    show(d, "trafficModel3")
    d foreach {
      case (Crash,   p) => p should be (0.0891 plusOrMinus 1e-11)
      case (NoCrash, p) => p should be (0.9109 plusOrMinus 1e-11)
    }
  }
}

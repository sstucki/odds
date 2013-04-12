package odds

import org.scalatest.FlatSpec

trait TrafficModel extends OddsLang {
  val lightsIt = Iterator("Red", "Yellow", "Green")
  val actionIt = Iterator("Stop", "Drive")
  val resultIt = Iterator("Crash", "NoCrash")

  val Red, Yellow, Green = lightsIt.next
  val Stop, Drive        = actionIt.next
  val Crash, NoCrash     = resultIt.next

  type Light = String
  type Action = String
  type Driver = Rand[Light] => Rand[Action]


  val trafficLight = choice(Red -> 0.5, Yellow -> 0.1, Green -> 0.4) dbg "light"

  def otherLight(light: Rand[Light]) = light map {
    case Red => Green
    case Yellow => Red
    case Green => Red
  } dbg "otherLight"
  def cautiousDriver(light: Rand[Light]) = light flatMap {
    case Red => always(Stop)
    case Yellow => choice(Stop -> 0.9, Drive -> 0.1)
    case Green => always(Drive)
  } dbg "cautiousDriver"
  def aggressiveDriver(light: Rand[Light]) = light flatMap {
    case Red => choice(Stop -> 0.9, Drive -> 0.1)
    case Yellow => choice(Stop -> 0.1, Drive -> 0.9)
    case Green => always(Drive)
  } dbg "aggressiveDriver"

  def crash(driver1: Driver, driver2: Driver, light: Rand[Light]) = {
    light flatMap { l =>
      val light = always(l)

      val d1 = driver1(light)
      val d2 = driver2(otherLight(light))
      (driver1(light) === always(Drive) && (driver2(otherLight(light)) === always(Drive))) flatMap {
        case true =>
          choice(Crash -> 0.9, NoCrash -> 0.1)
        case _ =>
          always(NoCrash)
      }
    }
  }

  def crash2(driver1: Driver, driver2: Driver, light: Rand[Light]) = {
    (driver1(light) === always(Drive) && (driver2(otherLight(light)) === always(Drive))) flatMap {
      case true =>
        choice(Crash -> 0.9, NoCrash -> 0.1) dbg "result"
      case _ =>
        always(NoCrash)
    }
  }
}

trait TrafficModelTest
    extends TrafficModel
    with OddsExact
    with OddsPrettyPrint
    with FlatSpec {

  behavior of "TrafficModel"

  it should "show the results of trafficModel" in {
    val trafficModel = crash(cautiousDriver, aggressiveDriver, trafficLight)
    show(trafficModel, "trafficModel")
  }

  it should "show the results of trafficModel2" in {
    val trafficModel2 = crash(aggressiveDriver, aggressiveDriver, trafficLight)
    show(trafficModel2, "trafficModel2")
  }

  it should "show the results of trafficModel3" in {
    val trafficModel3 = crash2(cautiousDriver, aggressiveDriver, trafficLight)
    show(trafficModel3, "trafficModel3")
  }

  it should "show the results of trafficModel4" in {
    val trafficModel4 = crash2(aggressiveDriver, aggressiveDriver, trafficLight)
    show(trafficModel4, "trafficModel4")
  }
}

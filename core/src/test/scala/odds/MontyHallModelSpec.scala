package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import inference._

trait MontyHallModel extends OddsLang {

  def randomDoor = uniform(1, 2, 3)

  // Choose uniformly among remaining doors
  def remainingDoor = Rand { (a: Int, b: Int) =>
    val remainingDoors = List(1, 2, 3) diff List(a, b)
    uniform(remainingDoors: _*)
  }

  val priceDoor   = randomDoor // the door hiding the price
  val firstChoice = randomDoor // the door you pick

  // Monty opens a goat door you didn't pick
  val openDoor = remainingDoor(priceDoor, firstChoice)

  // You switch doors
  val secondChoice = remainingDoor(firstChoice, openDoor)

  val firstChoiceWins  = firstChoice  == priceDoor
  val secondChoiceWins = secondChoice == priceDoor
}

trait MontyHallMonadicModel extends OddsLang {

  import Rand.ToScalaMonadic

  def randomDoor = uniform(1, 2, 3)

  // Choose uniformly among remaining doors
  def remainingDoor(a: Int, b: Int): Rand[Int] = {
    val remainingDoors = List(1, 2, 3) diff List(a, b)
    uniform(remainingDoors: _*)
  }

  def monty = for {

    priceDoor   <- randomDoor // the door hiding the price
    firstChoice <- randomDoor // the door you pick

    // Monty opens a goat door you didn't pick
    openDoor <- remainingDoor(priceDoor, firstChoice)

    // You switch doors
    secondChoice <- remainingDoor(firstChoice, openDoor)

  } yield (firstChoice == priceDoor, secondChoice == priceDoor)

  val firstChoiceWins  = for ((c1, _) <- monty) yield c1
  val secondChoiceWins = for ((_, c2) <- monty) yield c2
}

class MontyHallModelSpec
    extends MontyHallModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpecLike
    with Matchers {

  behavior of "MontyHallModel"

  it should "show the first choice" in {
    val d = reify(firstChoiceWins)
    show(d, "first choice wins")
    d foreach {
      case (true, p)  => p should be (1.0 / 3.0 plusOrMinus 1e-12)
      case (false, p) => p should be (2.0 / 3.0 plusOrMinus 1e-12)
    }
  }

  it should "show the second choice" in {
    val d = reify(secondChoiceWins)
    show(d, "second choice wins")
    d foreach {
      case (true, p)  => p should be (2.0 / 3.0 plusOrMinus 1e-12)
      case (false, p) => p should be (1.0 / 3.0 plusOrMinus 1e-12)
    }
  }
}

class MontyHallMonadicModelSpec
    extends MontyHallMonadicModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpecLike
    with Matchers {

  behavior of "MontyHallMonadicModel"

  it should "show the first choice" in {
    val d = reify(firstChoiceWins)
    show(d, "first choice wins")
    d foreach {
      case (true, p)  => p should be (1.0 / 3.0 plusOrMinus 1e-12)
      case (false, p) => p should be (2.0 / 3.0 plusOrMinus 1e-12)
    }
  }

  it should "show the second choice" in {
    val d = reify(secondChoiceWins)
    show(d, "second choice wins")
    d foreach {
      case (true, p)  => p should be (2.0 / 3.0 plusOrMinus 1e-12)
      case (false, p) => p should be (1.0 / 3.0 plusOrMinus 1e-12)
    }
  }
}

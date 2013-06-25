package odds

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

trait MontyHallModel extends OddsLang {

  def randomDoor = uniform(1, 2, 3)

  // Choose uniformly among remaining doors
  def remainingDoor(a: Rand[Int], b: Rand[Int]): Rand[Int] = for {
    av <- a
    bv <- b
    c <- uniform(List(1, 2, 3) diff List(av, bv): _*)
  } yield c

  val priceDoor   = randomDoor // the door hiding the price
  val firstChoice = randomDoor // the door you pick

  // Monty opens a goat door you didn't pick
  val openDoor = remainingDoor(priceDoor, firstChoice)

  // You switch doors
  val secondChoice = remainingDoor(firstChoice, openDoor)

  val firstChoiceWins  = firstChoice  === priceDoor
  val secondChoiceWins = secondChoice === priceDoor
}

trait MontyHallMonadicModel extends OddsLang {

  def randomDoor = uniform(1, 2, 3)

  // Choose uniformly among remaining doors
  def remainingDoor(a: Int, b: Int): Rand[Int] =
    uniform(List(1, 2, 3) diff List(a, b): _*)

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

class MontyHallModelTest
    extends MontyHallModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpec
    with ShouldMatchers {

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

class MontyHallMonadicModelTest
    extends MontyHallMonadicModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpec
    with ShouldMatchers {

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

package odds

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

trait MontyHallModel extends OddsLang {

  def randomDoor  = uniform(1, 2, 3)
  val priceDoor   = randomDoor // the door hiding the price
  val firstChoice = randomDoor // the door you pick

  // Monty opens a goat door you didn't pick
  val openDoor = (priceDoor, firstChoice) flatMap {
    case (p, f) =>
      val candidates = List(1, 2, 3) diff List(p, f)
      uniform(candidates: _*)
  }

  // you switch doors
  val secondChoice = (firstChoice, openDoor) flatMap {
    case (f, o) =>
      val lastDoor = (List(1, 2, 3) diff List(f, o)).head
      always(lastDoor)
  }

  val firstChoiceWins  = firstChoice  === priceDoor
  val secondChoiceWins = secondChoice === priceDoor
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

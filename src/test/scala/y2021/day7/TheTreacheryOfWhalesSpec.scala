package y2021.day7

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TheTreacheryOfWhalesSpec extends AnyWordSpec with should.Matchers {
  "Calculate optimum position" should {
    "not move when a single position is provided" in {
      TheTreacheryOfWhales.calculateOptimumPosition(List(5)) should be(0)
    }

    "move to the optimum position for a list of inputs" in {
      val input: List[Int] = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
      TheTreacheryOfWhales.calculateOptimumPosition(input) should be(37)
    }
  }

  "Calculate optimum position (with updated fuel calculation)" should {
    "not move when a single position is provided" in {
      TheTreacheryOfWhales.calculateOptimumPositionWithUpdatedFuelCalculation(List(5)) should be(0)
    }

    "calculate fuel use using " in {
      TheTreacheryOfWhales.calculateOptimumPositionWithUpdatedFuelCalculation(List(5, 9)) should be(6)
    }

    "move to the optimum position for a list of inputs" in {
      val input: List[Int] = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
      TheTreacheryOfWhales.calculateOptimumPositionWithUpdatedFuelCalculation(input) should be(168)
    }
  }
}

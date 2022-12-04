package y2021.day1

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SonarSweepSpec extends AnyWordSpec with should.Matchers {
  "Counting increases" should {
    "increment when subsequent depth is greater than the current one" in {
      val input = List(20, 30)
      SonarSweep.countIncreases(input) should be(1)
    }

    "not increment when subsequent depth is less than the current one" in {
      val input = List(20, 10)
      SonarSweep.countIncreases(input) should be(0)
    }

    "calculate number of times depth has increased in a list" in {
      val input = List(20, 30, 10, 40, 60, 50, 90, 30)
      SonarSweep.countIncreases(input) should be(4)
    }

    "return current counter value when single depth is provided" in {
      val input = List(1)
      SonarSweep.countIncreases(input, 10) should be(10)
    }
  }

  "Counting increases with window" should {
    "increment when sum of subsequent window is greater than the current one" in {
      val input = List(20, 30, 10, 40)
      SonarSweep.countWindowIncreases(input) should be(1)
    }

    "not increment when sum of subsequent window is greater than the current one" in {
      val input = List(20, 30, 10, 10)
      SonarSweep.countWindowIncreases(input) should be(0)
    }

    "calculate sum of increasing windows in a list" in {
      val input = List(20, 30, 10, 40, 60, 50, 90, 30)
      SonarSweep.countWindowIncreases(input) should be(4)
    }

    "return current counter when no subsequent window exists" in {
      val input = List(20, 30, 10)
      SonarSweep.countWindowIncreases(input, 10) should be(10)
    }
  }
}

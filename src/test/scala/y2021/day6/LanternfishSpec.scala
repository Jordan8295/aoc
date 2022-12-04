package y2021.day6

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class LanternfishSpec extends AnyWordSpec with should.Matchers {
  "Simulating lanternfish" should {
    "decrease timer when current time is >0" in {
      Lanternfish.simulateLanternfish(List(6, 8, 5, 3), day = 80) should be(4)
    }

    "spawn new lanternfish and reset timer when time is 0" in {
      Lanternfish.simulateLanternfish(List(6, 8, 0, 3, 0), day = 80) should be(7)
    }

    "continue until after day 80" in {
      Lanternfish.simulateLanternfish(List(6, 8, 5, 3), day = 79) should be(4)
    }
  }
}

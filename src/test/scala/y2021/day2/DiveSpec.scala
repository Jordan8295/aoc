package y2021.day2

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class DiveSpec extends AnyWordSpec with should.Matchers {
  "Forward" should {
    "increase horizontal position by magnitude and not alter depth" in {
      Forward(10).amendPosition((0, 0)) should be((10, 0))
    }

    "increase horizontal position by magnitude and increase depth by magnitude multiplied when using aim" in {
      Forward(10).amendPositionWithAim((0, 0, 10)) should be((10, 100, 10))
    }
  }

  "Down" should {
    "increase depth by magnitude" in {
      Down(5).amendPosition((0, 0)) should be((0, 5))
    }

    "increase aim by magnitude when using aim" in {
      Down(5).amendPositionWithAim((0, 10, 0)) should be((0, 10, 5))
    }
  }

  "Up" should {
    "decrease depth by magnitude" in {
      Up(5).amendPosition((0, 20)) should be((0, 15))
    }

    "decrease aim by magnitude when using aim" in {
      Up(5).amendPositionWithAim((0, 20, 50)) should be((0, 20, 45))
    }
  }
}

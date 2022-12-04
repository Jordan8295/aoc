package y2021.day5

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class HydrothermalVentureSpec extends AnyWordSpec with should.Matchers {
  "Coordinates covered by line" should {
    "return all coordinates covered by horizontal line" in {
      Line((0, 0), (5, 0)).coordinatesCovered should be(List((0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0)))
      Line((5, 0), (0, 0)).coordinatesCovered should be(List((5, 0), (4, 0), (3, 0), (2, 0), (1, 0), (0, 0)))
    }

    "return all coordinates covered by vertical line" in {
      Line((0, 0), (0, 5)).coordinatesCovered should be(List((0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5)))
      Line((0, 5), (0, 0)).coordinatesCovered should be(List((0, 5), (0, 4), (0, 3), (0, 2), (0, 1), (0, 0)))
    }

    "return all coordinates covered by a diagonal line" in {
      Line((0, 0), (5, 5)).coordinatesCovered should be(List((0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5)))
      Line((5, 5), (0, 0)).coordinatesCovered should be(List((5, 5), (4, 4), (3, 3), (2, 2), (1, 1), (0, 0)))
      Line((0, 5), (5, 0)).coordinatesCovered should be(List((0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)))
      Line((5, 0), (0, 5)).coordinatesCovered should be(List((5, 0), (4, 1), (3, 2), (2, 3), (1, 4), (0, 5)))
    }
  }

  "Count overlaps" should {
    "return 0 when no lines overlap" in {
      HydrothermalVenture.countOverlaps(List(Line((0, 0), (5, 0)), Line((0, 1), (5, 1)))) should be(0)
    }

    "return 1 when two lines overlap" in {
      HydrothermalVenture.countOverlaps(List(Line((0, 0), (5, 0)), Line((0, 0), (0, 5)))) should be(1)
    }

    "return total of overlapping lines" in {
      HydrothermalVenture.countOverlaps(List(Line((0, 0), (5, 0)), Line((0, 1), (5, 1)), Line((0, 0), (0, 5)))) should be(2)
    }
  }
}

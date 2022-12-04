package y2022.day4

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class CampCleanupSpec extends AnyWordSpec with should.Matchers {
  "Completely contained" should {
    "return 0 if one range is not a subset of the other for a single line" in {
      val input = List((9 to 12, 1 to 5))
      CampCleanup.completelyContained(input) should be(0)
    }

    "return 1 if one range is a subset of the other for a single line" in {
      val input = List((9 to 12, 11 to 12))
      CampCleanup.completelyContained(input) should be(1)
    }

    "return sum of lines where one range is a subset of the other for multiple lines" in {
      val input = List((9 to 12, 11 to 12), (9 to 12, 1 to 5), (10 to 90, 5 to 100))
      CampCleanup.completelyContained(input) should be(2)
    }
  }

  "Overlapping" should {
    "return 0 if one range does not intersect with the other for a single line" in {
      val input = List((9 to 12, 1 to 5))
      CampCleanup.overlapping(input) should be(0)
    }

    "return 1 if one range intersects with the other for a single line" in {
      val input = List((9 to 12, 11 to 13))
      CampCleanup.overlapping(input) should be(1)
    }

    "return sum of lines where one range is a subset of the other for multiple lines" in {
      val input = List((9 to 12, 11 to 13), (9 to 12, 1 to 5), (10 to 90, 20 to 100))
      CampCleanup.overlapping(input) should be(2)
    }
  }
}

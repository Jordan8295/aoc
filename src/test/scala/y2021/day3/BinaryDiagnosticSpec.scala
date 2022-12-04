package y2021.day3

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class BinaryDiagnosticSpec extends AnyWordSpec with should.Matchers {
  "Calculate gamma rate" should {
    "return most common digit in a binary list of single values" in {
      val input = List("1", "0", "1")
      BinaryDiagnostic.calculateGamma(input) shouldBe "1"
    }

    "return most common digit in a binary list with multiple values" in {
      val input = List("10110", "00110", "11101")
      BinaryDiagnostic.calculateGamma(input) shouldBe "10110"
    }
  }

  "Calculate epsilon rate" should {
    "return the least common digit in a binary list of single values" in {
      val input = List("1", "0", "1")
      BinaryDiagnostic.calculateEpsilon(input) shouldBe "0"
    }

    "return most common digit in a binary list with multiple values" in {
      val input = List("10110", "00110", "11101")
      BinaryDiagnostic.calculateEpsilon(input) shouldBe "01001"
    }
  }

  "Calculate oxygen generator rating" should {
    "return 1 when given a list of 0 and 1" in {
      val input = List("0", "1")
      BinaryDiagnostic.calculateOxygenGeneratorRating(input) should be("1")
    }

    "filter based on the most common bit-value, starting with the most significant bit" in {
      val input = List("10110", "00110", "11101")
      BinaryDiagnostic.calculateOxygenGeneratorRating(input) should be("11101")
    }

    "return remaining row when one left" in {
      val input = List("10110")
      BinaryDiagnostic.calculateOxygenGeneratorRating(input) should be("10110")
    }
  }

  "Calculate CO2 scrubber rating" should {
    "return 0 when given a list of 0 and 1" in {
      val input = List("0", "1")
      BinaryDiagnostic.calculateCO2ScrubberRating(input) should be("0")
    }

    "filter based on the least common bit-value, starting with the most significant bit" in {
      val input = List("10110", "00110", "11101")
      BinaryDiagnostic.calculateCO2ScrubberRating(input) should be("00110")
    }

    "return remaining row when one left" in {
      val input = List("10110")
      BinaryDiagnostic.calculateCO2ScrubberRating(input) should be("10110")
    }
  }
}

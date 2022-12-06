package y2021.day8

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SevenSegmentSearchSpec extends AnyWordSpec with should.Matchers {
  "Count simple entries" should {
    "count 1" in {
      SevenSegmentSearch.countSimpleDigitsInOutput(List(Entry(Nil, List("ab", "abcde")))) should be(1)
    }

    "count 4" in {
      SevenSegmentSearch.countSimpleDigitsInOutput(List(Entry(Nil, List("abcd", "abcde")))) should be(1)
    }

    "count 7" in {
      SevenSegmentSearch.countSimpleDigitsInOutput(List(Entry(Nil, List("abc", "abcde")))) should be(1)
    }

    "count 8" in {
      SevenSegmentSearch.countSimpleDigitsInOutput(List(Entry(Nil, List("abcdefg", "abcde")))) should be(1)
    }
  }

  "Map digits" should {
    "calculate correct digits" in {
      val digits = SevenSegmentSearch.mapDigits(List(
        "acedgfb",
        "cdfbe",
        "gcdfa",
        "fbcad",
        "dab",
        "cefabd",
        "cdfgeb",
        "eafb",
        "cagedb",
        "ab")
      )

      digits(Zero) should be("cagedb".toSet)
      digits(One) should be("ab".toSet)
      digits(Two) should be("gcdfa".toSet)
      digits(Three) should be("fbcad".toSet)
      digits(Four) should be("eafb".toSet)
      digits(Five) should be("cdfbe".toSet)
      digits(Six) should be("cdfgeb".toSet)
      digits(Seven) should be("dab".toSet)
      digits(Eight) should be("acedgfb".toSet)
      digits(Nine) should be("cefabd".toSet)
    }
  }

  "Sum outputs" should {
    "add the sum of two outputs" in {
      val entry1 = Entry(
        List(
          "acedgfb",
          "cdfbe",
          "gcdfa",
          "fbcad",
          "dab",
          "cefabd",
          "cdfgeb",
          "eafb",
          "cagedb",
          "ab"
        ),
        List(
          "cdfeb",
          "fcadb",
          "cdfeb",
          "cdbaf"
        )
      )

      val entry2 = Entry(
        List(
          "be",
          "cfbegad",
          "cbdgef",
          "fgaecd",
          "cgeb",
          "fdcge",
          "agebfd",
          "fecdb",
          "fabcd",
          "edb"
        ),
        List(
          "fdgacbe",
          "cefdb",
          "cefbgd",
          "gcbe"
        )
      )

      val entries = List(entry1, entry2)
      SevenSegmentSearch.sumOutputs(entries) should be(5353 +  8294)
    }
  }
}

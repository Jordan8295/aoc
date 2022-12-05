package y2021.day8

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SevenSegmentSearchSpec extends AnyWordSpec with should.Matchers {
  "Count simple entries should count 1" in {
    SevenSegmentSearch.countSimpleDigitsInOutput(List(Entry(Nil, List("ab", "abcde")))) should be(1)
  }

  "Count simple entries should count 4" in {
    SevenSegmentSearch.countSimpleDigitsInOutput(List(Entry(Nil, List("abcd", "abcde")))) should be(1)
  }

  "Count simple entries should count 7" in {
    SevenSegmentSearch.countSimpleDigitsInOutput(List(Entry(Nil, List("abc", "abcde")))) should be(1)
  }

  "Count simple entries should count 8" in {
    SevenSegmentSearch.countSimpleDigitsInOutput(List(Entry(Nil, List("abcdefg", "abcde")))) should be(1)
  }
}

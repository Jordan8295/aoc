package y2022.day6

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class TuningTroubleSpec extends AnyWordSpec with should.Matchers {
  "Find marker" should {
    "return first occurrance of 4 consecutive different letters" in {
      TuningTrouble.findStartOfPacketMarker("mjqjpqmgbljsphdztnvjfqwrcgsmlb") should be(7)
      TuningTrouble.findStartOfPacketMarker("bvwbjplbgvbhsrlpgdmjqwftvncz") should be(5)
      TuningTrouble.findStartOfPacketMarker("nppdvjthqldpwncqszvftbrmjlhg") should be(6)
      TuningTrouble.findStartOfPacketMarker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") should be(10)
      TuningTrouble.findStartOfPacketMarker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") should be(11)
    }

    "return first occurrance of 14 consecutive different letters" in {
      TuningTrouble.findStartOfMessageMarker("mjqjpqmgbljsphdztnvjfqwrcgsmlb") should be(19)
      TuningTrouble.findStartOfMessageMarker("bvwbjplbgvbhsrlpgdmjqwftvncz") should be(23)
      TuningTrouble.findStartOfMessageMarker("nppdvjthqldpwncqszvftbrmjlhg") should be(23)
      TuningTrouble.findStartOfMessageMarker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") should be(29)
      TuningTrouble.findStartOfMessageMarker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") should be(26)
    }
  }
}

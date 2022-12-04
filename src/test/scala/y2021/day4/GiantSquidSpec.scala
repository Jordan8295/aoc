package y2021.day4

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class GiantSquidSpec extends AnyWordSpec with should.Matchers {
  val bingoCard: BingoCard = BingoCard(List(
    List(1, 20, 5, 78, 6),
    List(9, 88, 32, 4, 3),
    List(69, 62, 99, 7, 8),
    List(15, 16, 17, 21, 24),
    List(59, 61, 55, 54, 53)
  ))

  "A bingo card has won" should {
    "be false when no numbers have been called" in {
      bingoCard.hasWon(Nil) should be(false)
    }

    "be false when numbers have been called but no row/column is complete" in {
      bingoCard.hasWon(List(1, 9, 69, 15, 20, 5, 78)) should be(false)
    }

    "be true when a row is complete" in {
      bingoCard.hasWon(List(1, 20, 5, 78, 6)) should be(true)
      bingoCard.hasWon(List(9, 88, 32, 4, 3)) should be(true)
      bingoCard.hasWon(List(69, 62, 99, 7, 8)) should be(true)
      bingoCard.hasWon(List(15, 16, 17, 21, 24)) should be(true)
      bingoCard.hasWon(List(59, 61, 55, 54, 53)) should be(true)
    }

    "be true when a column is complete" in {
      bingoCard.hasWon(List(1, 9, 69, 15, 59)) should be(true)
      bingoCard.hasWon(List(20, 88, 62, 16, 61)) should be(true)
      bingoCard.hasWon(List(5, 32, 99, 17, 55)) should be(true)
      bingoCard.hasWon(List(78, 4, 7, 21, 54)) should be(true)
      bingoCard.hasWon(List(6, 3, 8, 24, 53)) should be(true)
    }
  }

  "Unmarked numbers on a bingo card" should {
    "return all numbers on a board if no numbers have been called" in {
      bingoCard.unmarkedNumbers(Nil) should be(
        Set(1, 20, 5, 78, 6, 9, 88, 32, 4, 3, 69, 62, 99, 7, 8, 15, 16, 17, 21, 24, 59, 61, 55, 54, 53)
      )
    }

    "return remaining numbers on board which have not been called" in {
      bingoCard.unmarkedNumbers(List(1, 20, 5, 78, 6, 9, 88, 32, 4, 3, 69, 62, 99, 7, 8, 15, 16, 17, 21, 24)) should be(
        Set(59, 61, 55, 54, 53)
      )
    }
  }

  "Calculate winner" should {
    "return first board to win along with last number called" in {
      val secondBingoCard = BingoCard(List(
        List(1, 20, 5, 78, 3),
        List(9, 88, 32, 4, 3),
        List(69, 62, 99, 7, 8),
        List(15, 16, 17, 21, 24),
        List(59, 61, 55, 54, 53)
      ))

      GiantSquid.calculateWinner(List(1, 20, 78, 6, 5, 3), List(bingoCard, secondBingoCard)) should be(
        (List(1, 20, 78, 6, 5), bingoCard)
      )
    }
  }

  "Calculate loser" should {
    "return last board to win along with last number called" in {
      val secondBingoCard = BingoCard(List(
        List(1, 20, 5, 78, 3),
        List(9, 88, 32, 4, 3),
        List(69, 62, 99, 7, 8),
        List(15, 16, 17, 21, 24),
        List(59, 61, 55, 54, 53)
      ))

      GiantSquid.calculateLoser(List(1, 20, 78, 6, 5, 3), List(bingoCard, secondBingoCard)) should be(
        (List(1, 20, 78, 6, 5, 3), secondBingoCard)
      )
    }
  }
}

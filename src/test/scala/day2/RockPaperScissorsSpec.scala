package day2

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class RockPaperScissorsSpec extends AnyWordSpec with should.Matchers {
  "Against rock" should {
    "paper should score 8" in {
      Rock.vs(Paper) should be(8)
    }

    "rock should score 4" in {
      Rock.vs(Rock) should be(4)
    }

    "scissors score 3" in {
      Rock.vs(Scissors) should be(3)
    }

    "losing move should be calculated as scissors" in {
      Rock.codeToMove("X") should be(Scissors)
    }

    "drawing move should be calculated as rock" in {
      Rock.codeToMove("Y") should be(Rock)
    }

    "winning move should be calculated as paper" in {
      Rock.codeToMove("Z") should be(Paper)
    }
  }

  "Against paper" should {
    "scissors should score 9" in {
      Paper.vs(Scissors) should be(9)
    }

    "paper should score 5" in {
      Paper.vs(Paper) should be(5)
    }

    "rock should score 1" in {
      Paper.vs(Rock) should be(1)
    }

    "losing move should be calculated as rock" in {
      Paper.codeToMove("X") should be(Rock)
    }

    "drawing move should be calculated as paper" in {
      Paper.codeToMove("Y") should be(Paper)
    }

    "winning move should be calculated as scissors" in {
      Paper.codeToMove("Z") should be(Scissors)
    }
  }

  "Against scissors" should {
    "rock should score 7" in {
      Scissors.vs(Rock) should be(7)
    }

    "scissors should score 6" in {
      Scissors.vs(Scissors) should be(6)
    }

    "paper should score 2" in {
      Scissors.vs(Paper) should be(2)
    }

    "losing move should be calculated as paper" in {
      Scissors.codeToMove("X") should be(Paper)
    }

    "drawing move should be calculated as scissors" in {
      Scissors.codeToMove("Y") should be(Scissors)
    }

    "winning move should be calculated as rock" in {
      Scissors.codeToMove("Z") should be(Rock)
    }
  }

  "Calculate the score" should {
    "sum the scores for each individual game" in {
      val input = List(List("A", "Z"), List("B", "Y"), List("C", "X"))
      RockPaperScissors.calculateScore(input) should be(15)
    }
  }

  "Calculating the score with updated tactics" should {
    "sum the scores for each individual game" in {
      val input = List(List("A", "Z"), List("B", "Y"), List("C", "X"))
      RockPaperScissors.calculateScoreUpdatedTactics(input) should be(15)
    }
  }

}

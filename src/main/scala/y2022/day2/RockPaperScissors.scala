package y2022.day2

import utils.FileReader

trait Move {
  val score: Int
  def vs(move: Move): Int
  def codeToMove(code: String): Move
}

case object Rock extends Move {
  override val score: Int = 1

  override def vs(move: Move): Int = move.score + (move match {
    case Rock => Draw.score
    case Paper => Win.score
    case Scissors => Loss.score
  })

  def codeToMove(code: String): Move = code match {
    case "X" => Scissors
    case "Y" => Rock
    case "Z" => Paper
  }
}

case object Paper extends Move {
  override val score: Int = 2

  override def vs(move: Move): Int = move.score + (move match {
    case Rock => Loss.score
    case Paper => Draw.score
    case Scissors => Win.score
  })

  def codeToMove(code: String): Move = code match {
    case "X" => Rock
    case "Y" => Paper
    case "Z" => Scissors
  }
}

case object Scissors extends Move {
  override val score: Int = 3

  override def vs(move: Move): Int = move.score + (move match {
    case Rock => Win.score
    case Paper => Loss.score
    case Scissors => Draw.score
  })

  def codeToMove(code: String): Move = code match {
    case "X" => Paper
    case "Y" => Scissors
    case "Z" => Rock
  }
}

trait Result {
  val score: Int
}

case object Win extends Result {
  val score: Int = 6
}

case object Draw extends Result {
  val score: Int = 3
}

case object Loss extends Result {
  val score: Int = 0
}

object RockPaperScissors extends App {

  def calculateScore(input: List[List[String]]): Int = input
    .map(_.map {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
    })
    .map(game => (game.head, game.last)).map(game => game._1.vs(game._2)).sum

  def calculateScoreUpdatedTactics(input: List[List[String]]): Int = input
    .map {
      case "A" :: code :: Nil => (Rock, Rock.codeToMove(code))
      case "B" :: code :: Nil => (Paper, Paper.codeToMove(code))
      case "C" :: code :: Nil => (Scissors, Scissors.codeToMove(code))
    }
    .map(game => game._1.vs(game._2)).sum

  val input: List[List[String]] = FileReader.readInput("y2022/day2").map(_.split(" ").toList)

  println(calculateScore(input))
  println(calculateScoreUpdatedTactics(input))
}

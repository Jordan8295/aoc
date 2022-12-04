package y2021.day4

import utils.FileReader

import scala.annotation.tailrec
import scala.util.matching.Regex

case class BingoCard(numbers: List[List[Int]]) {
  def hasWon(calledNumbers: List[Int]): Boolean =
    numbers.exists(row => row.count(calledNumbers.contains(_)) == 5) ||
      numbers.transpose.exists(column => column.count(calledNumbers.contains(_)) == 5)

  def unmarkedNumbers(calledNumbers: List[Int]): Set[Int] = numbers.flatten.diff(calledNumbers).toSet
}

object GiantSquid extends App {

  @tailrec
  def calculateWinner(numbers: List[Int], bingoCards: List[BingoCard], numberOfCalls: Int = 0): (List[Int], BingoCard) =
    if (bingoCards.exists(_.hasWon(numbers.take(numberOfCalls + 1)))) {
      ((numbers.take(numberOfCalls + 1), bingoCards.find(_.hasWon(numbers.take(numberOfCalls + 1))).get))
    } else {
      calculateWinner(numbers, bingoCards, numberOfCalls + 1)
    }

  @tailrec
  def calculateLoser(numbers: List[Int], bingoCards: List[BingoCard], numberOfCalls: Int = 0): (List[Int], BingoCard) =
    if (bingoCards.exists(!_.hasWon(numbers.take(numberOfCalls + 1)))) {
      calculateLoser(numbers, bingoCards.filterNot(_.hasWon(numbers.take(numberOfCalls + 1))), numberOfCalls + 1)
    } else {
      (numbers.take(numberOfCalls + 1), bingoCards.head)
    }

  val input: List[String] = FileReader.readInput("y2021/day4")
  val numbers: List[Int] = input.head.split(",").toList.map(_.toInt)
  val rowRegex: Regex = "[ ]?(\\d+)[ ]+(\\d+)[ ]+(\\d+)[ ]+(\\d+)[ ]+(\\d+)".r
  val bingoCards: List[BingoCard] =
    input
      .drop(2)
      .filterNot(_ == "")
      .map {
        case rowRegex(a, b, c, d, e) => List(a.toInt, b.toInt, c.toInt, d.toInt, e.toInt)
      }
      .grouped(5)
      .toList
      .map(card => BingoCard(card))

  val winner = calculateWinner(numbers, bingoCards)
  val loser = calculateLoser(numbers, bingoCards)

  println(winner._1.last * winner._2.unmarkedNumbers(winner._1).sum)
  println(loser._1.last * loser._2.unmarkedNumbers(loser._1).sum)
}

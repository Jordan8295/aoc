package y2021.day8

import utils.FileReader

import scala.util.matching.Regex

case class Entry(signalPatterns: List[String], outputValue: List[String])

sealed trait Digit {
  val segmentsLength: Int
  def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char]
  def toString: String
}

case object Zero extends Digit {
  override val segmentsLength: Int = 6

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns
      .map(_.toSet)
      .find(signal =>
        signal.intersect(knownDigits(One)).size == One.segmentsLength &&
          signal != knownDigits(Nine) &&
          signal.size == segmentsLength
      ).get

  override def toString: String = "0"
}

case object One extends Digit {
  override val segmentsLength: Int = 2

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns.find(_.length == segmentsLength).get.toSet

  override def toString: String = "1"
}

case object Two extends Digit {
  override val segmentsLength: Int = 5

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns
      .map(_.toSet)
      .find(signal =>
        signal.size == segmentsLength &&
          signal != knownDigits(Three) &&
          signal != knownDigits(Five)
      ).get

  override def toString: String = "2"
}

case object Three extends Digit {
  override val segmentsLength: Int = 5

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns
      .map(_.toSet)
      .find(signal =>
        signal.intersect(knownDigits(Nine)).size == Three.segmentsLength &&
          signal.intersect(knownDigits(One)).size == One.segmentsLength &&
          signal.size == segmentsLength
      ).get

  override def toString: String = "3"
}

case object Four extends Digit {
  override val segmentsLength: Int = 4

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns.find(_.length == segmentsLength).get.toSet

  override def toString: String = "4"
}

case object Five extends Digit {
  override val segmentsLength: Int = 5

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns
      .map(_.toSet)
      .find(signal =>
        signal.intersect(knownDigits(Nine)).size == Three.segmentsLength &&
          signal.intersect(knownDigits(One)).size != One.segmentsLength &&
          signal.size == segmentsLength
      ).get

  override def toString: String = "5"
}

case object Six extends Digit {
  override val segmentsLength: Int = 6

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns
      .map(_.toSet)
      .find(signal =>
        !knownDigits.values.toList.contains(signal)
      ).get

  override def toString: String = "6"
}

case object Seven extends Digit {
  override val segmentsLength: Int = 3

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns.find(_.length == segmentsLength).get.toSet

  override def toString: String = "7"
}

case object Eight extends Digit {
  override val segmentsLength: Int = 7

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns.find(_.length == segmentsLength).get.toSet

  override def toString: String = "8"
}

case object Nine extends Digit {
  override val segmentsLength: Int = 6

  override def evaluate(signalPatterns: List[String], knownDigits: Map[Digit, Set[Char]]): Set[Char] =
    signalPatterns
      .map(_.toSet)
      .find(signal =>
        signal.intersect(knownDigits(Four)).size == Four.segmentsLength &&
          signal.size == segmentsLength
      ).get

  override def toString: String = "9"
}

object SevenSegmentSearch extends App {
  def countSimpleDigitsInOutput(input: List[Entry]): Int = input.map {
    entry => entry.outputValue.count(
      output =>
        List(One.segmentsLength, Four.segmentsLength, Seven.segmentsLength, Eight.segmentsLength).contains(output.length))
  }.sum

  def mapDigits(signalPatterns: List[String]): Map[Digit, Set[Char]] =
    List(
      One,
      Four,
      Seven,
      Eight,
      Nine,
      Three,
      Five,
      Two,
      Zero,
      Six
    ).foldLeft(Map.empty[Digit, Set[Char]]) {
      (digitMap, digit) =>
        digitMap + (digit -> digit.evaluate(signalPatterns, digitMap))
    }

  def sumOutputs(input: List[Entry]): Int = input.map {
    entry =>
      (mapDigits(entry.signalPatterns), entry.outputValue)
  }.map {
    x =>
      Integer.parseInt(x._2
        .map(output =>
          x._1.find(_._2 == output.toSet).get._1.toString
        ).mkString)
  }.sum

  val inputPattern: Regex = "([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) \\| ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+)".r
  val input: List[Entry] = FileReader.readInput("y2021/day8").map {
    case inputPattern(a, b, c, d, e, f, g, h, i, j, w, x, y, z) => Entry(List(a, b, c, d, e, f, g, h, i, j), List(w, x, y, z))
  }

  println(countSimpleDigitsInOutput(input))
  println(sumOutputs(input))

}

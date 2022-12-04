package y2021.day3

import utils.FileReader

import scala.annotation.tailrec

object BinaryDiagnostic extends App {

  def calculateGamma(input: List[String]): String = input.foldLeft(List.fill(input.head.length)(0)) {
    (counters, row) =>
      counters.zipWithIndex.map(count => if (row.charAt(count._2) == '1') count._1 + 1 else count._1)
  }.map {
    case count if count > input.length / 2 => '1'
    case _ => '0'
  }.mkString

  def calculateEpsilon(input: List[String]): String = calculateGamma(input).map {
    case '1' => '0'
    case _ => '1'
  }.mkString

  @tailrec
  def calculateOxygenGeneratorRating(input: List[String], index: Int = 0): String = input match {
    case row :: Nil => row
    case inputs if inputs.map(_.charAt(index)).count(_ == '1') >= inputs.length.toFloat / 2 =>
      calculateOxygenGeneratorRating(inputs.filter(_.charAt(index) == '1'), index + 1)
    case inputs => calculateOxygenGeneratorRating(inputs.filter(_.charAt(index) == '0'), index + 1)
  }

  @tailrec
  def calculateCO2ScrubberRating(input: List[String], index: Int = 0): String = input match {
    case row :: Nil => row
    case inputs if inputs.map(_.charAt(index)).count(_ == '1') < (inputs.length.toFloat / 2) =>
      calculateCO2ScrubberRating(inputs.filter(_.charAt(index) == '1'), index + 1)
    case inputs => calculateCO2ScrubberRating(inputs.filter(_.charAt(index) == '0'), index + 1)
  }

  val input: List[String] = FileReader.readInput("y2021/day3")

  println(Integer.parseInt(calculateGamma(input), 2) * Integer.parseInt(calculateEpsilon(input), 2))
  println(Integer.parseInt(calculateOxygenGeneratorRating(input), 2) * Integer.parseInt(calculateCO2ScrubberRating(input), 2))
}

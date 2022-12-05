package y2021.day8

import utils.FileReader

import scala.util.matching.Regex

case class Entry(signalPatterns: List[String], outputValue: List[String])

object SevenSegmentSearch extends App {
  def countSimpleDigitsInOutput(input: List[Entry]): Int = input.map {
    entry => entry.outputValue.count(output => List(2, 3, 4, 7).contains(output.length))
  }.sum

  val inputPattern: Regex = "([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+) \\| ([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+)".r
  val input: List[Entry] = FileReader.readInput("y2021/day8").map {
    case inputPattern(a, b, c, d, e, f, g, h, i, j, w, x, y, z) => Entry(List(a, b, c, d, e, f, g, h, i, j), List(w, x, y, z))
  }

  println(countSimpleDigitsInOutput(input))
}

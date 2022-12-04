package y2022.day4

import utils.FileReader

import scala.util.matching.Regex

object CampCleanup extends App {
  def completelyContained(ranges: List[(Range, Range)]): Int = ranges.count {
    case r => r._1.forall(section => r._2.contains(section)) || r._2.forall(section => r._1.contains(section))
  }

  def overlapping(ranges: List[(Range, Range)]): Int = ranges.count {
    case r => r._1.exists(section => r._2.contains(section))
  }

  val linePattern: Regex = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r
  val input: List[(Range, Range)] = FileReader.readInput("y2022/day4").map{
    case linePattern(a, b, x, y) => (a.toInt to b.toInt, x.toInt to y.toInt)
  }

  println(completelyContained(input))
  println(overlapping(input))
}

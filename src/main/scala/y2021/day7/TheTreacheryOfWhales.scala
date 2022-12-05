package y2021.day7

import utils.FileReader


object TheTreacheryOfWhales extends App {
  def calculateOptimumPosition(input: List[Int]): Int =
    (input.min to input.max).map {
      position =>
        input.map(p => (p - position).abs).sum
    }.min

  def calculateOptimumPositionWithUpdatedFuelCalculation(input: List[Int]): Int =
    (input.min to input.max).map {
      position =>
        input
          .map(p => (p - position).abs)
          .map(p => (1 to p).sum).sum
    }.min

  val input: List[Int] = FileReader.readInput("y2021/day7").head.split(",").map(_.toInt).toList

  println(calculateOptimumPosition(input))
  println(calculateOptimumPositionWithUpdatedFuelCalculation(input))
}

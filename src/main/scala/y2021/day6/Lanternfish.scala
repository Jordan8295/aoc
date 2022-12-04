package y2021.day6

import utils.FileReader

import scala.annotation.tailrec

object Lanternfish extends App {

  def simulateLanternfish(input: List[Int], day: Int = 1, maxDays: Int = 80): Long = {
    @tailrec
    def simulateWithMap(timerMap: Map[Int, Long], day: Int, maxDays: Int): Long = {
      val newFish = timerMap.getOrElse[Long](0, 0)
      val updatedMap: Map[Int, Long] = {
        timerMap.filterNot(_._1 == 0).map {
          case (n, total) => (n - 1, total)
        } + (8 -> newFish, 6 -> (newFish + timerMap.getOrElse[Long](7, 0)))
      }

      if (day < maxDays) simulateWithMap(updatedMap, day + 1, maxDays)
      else {
        updatedMap.values.sum
      }
    }

    val timerMap: Map[Int, Long] = input.groupBy(identity).map(time => time._1 -> time._2.size.toLong)
    simulateWithMap(timerMap, day, maxDays)
  }

  val input: List[Int] = FileReader.readInput("y2021/day6").head.split(",").map(_.toInt).toList

  println(simulateLanternfish(input))
  println(simulateLanternfish(input, maxDays = 256))
}

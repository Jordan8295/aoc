package day1

import scala.io.Source
import scala.util.Using

object CalorieCounting extends App {
  def maxCalories(input: List[List[Int]]): Int = {
    input.map(_.sum).max
  }

  def top3MaxCalories(input: List[List[Int]]): Int = {
    input.map(_.sum).sorted.takeRight(3).sum
  }

  val input = Using(Source.fromFile("/Users/jolney/Projects/aoc22/src/main/scala/day1/input.txt")) {
    _.getLines.toList.foldLeft(List(List.empty[Int])) {
      case (l, item) if item == "" => l :+ List.empty
      case (l, item) =>
        val tail = l.last
        l.dropRight(1) :+ (tail :+ item.toInt)

    }
  }.get

  println(maxCalories(input))
  println(top3MaxCalories(input))

}

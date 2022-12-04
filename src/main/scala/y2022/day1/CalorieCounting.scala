package y2022.day1

import utils.FileReader

object CalorieCounting extends App {
  def maxCalories(input: List[List[Int]]): Int = {
    input.map(_.sum).max
  }

  def top3MaxCalories(input: List[List[Int]]): Int = {
    input.map(_.sum).sorted.takeRight(3).sum
  }

  val input = FileReader.readInput("y2022/day1").foldLeft(List(List.empty[Int])) {
    case (l, item) if item == "" => l :+ List.empty
    case (l, item) =>
      val tail = l.last
      l.dropRight(1) :+ (tail :+ item.toInt)
  }

  println(maxCalories(input))
  println(top3MaxCalories(input))
}

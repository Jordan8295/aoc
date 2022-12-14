package y2022.day3

import utils.FileReader

object RucksackReorganisation extends App {
  def findErrors(bags: List[(String, String)]): List[Char] = bags.map(
    bag =>
      bag._1.find(item => bag._2.exists(_ == item)).get
  )

  def findBadges(groups: List[List[String]]): List[Char] = groups.map(
    group =>
      group.head.find(item => group(1).exists(_ == item) && group.last.exists(_ == item)).get
  )

  def getPriority(items: List[Char]): Int = {
    items.map(_.toInt).map(priority => if (priority >= 97) priority - 96 else priority - 38).sum
  }

  val input: List[String] = FileReader.readInput("y2022/day3")

  val bags = input.map(bag => bag.splitAt(bag.length / 2))
  val groups = input.grouped(3).toList

  println(getPriority(findErrors(bags)))
  println(getPriority(findBadges(groups)))
}

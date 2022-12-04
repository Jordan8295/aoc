package y2021.day1

import utils.FileReader

import scala.annotation.tailrec

object SonarSweep extends App {
  @tailrec
  def countIncreases(depths: List[Int], counter: Int = 0): Int = depths match {
    case _ :: Nil => counter
    case depth :: tail if depth < tail.head => countIncreases(tail, counter + 1)
    case _ :: tail => countIncreases(tail, counter)
  }

  @tailrec
  def countWindowIncreases(depths: List[Int], counter: Int = 0): Int = depths match {
    case _ :: _ :: _ :: Nil => counter
    case a :: b :: c :: tail if a < tail.head => countWindowIncreases(b :: c :: tail, counter + 1)
    case _ :: tail => countWindowIncreases(tail, counter)
  }

  val input: List[Int] = FileReader.readInput("y2021/day1").map(_.toInt)

  println(countIncreases(input))
  println(countWindowIncreases(input))
}

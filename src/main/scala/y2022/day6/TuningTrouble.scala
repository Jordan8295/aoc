package y2022.day6

import utils.FileReader

import scala.annotation.tailrec
import scala.collection.immutable.::

object TuningTrouble extends App {

  @tailrec
  def findStartOfPacketMarker(input: String, index: Int = 4): Int = input.toList match {
    case a :: b :: c :: d :: tail if Set(a, b, c, d).size == 4 => index
    case _ :: tail  => findStartOfPacketMarker(tail.mkString, index + 1)
  }

  @tailrec
  def findStartOfMessageMarker(input: String, index: Int = 14): Int = input.toList match {
    case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: tail
      if Set(a, b, c, d, e, f, g, h, i, j, k, l, m, n).size == 14 => index
    case _ :: tail  => findStartOfMessageMarker(tail.mkString, index + 1)
  }

  val input: String = FileReader.readInput("y2022/day6").head

  println(findStartOfPacketMarker(input))
  println(findStartOfMessageMarker(input))
}

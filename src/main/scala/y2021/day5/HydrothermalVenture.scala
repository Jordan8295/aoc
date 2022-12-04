package y2021.day5

import utils.FileReader

import scala.annotation.tailrec
import scala.util.matching.Regex

case class Line(start: (Int, Int), end: (Int, Int)) {
  def coordinatesCovered: List[(Int, Int)] = {
    val xStep = if (end._1 - start._1 == 0) 0 else (end._1 - start._1) / (end._1 - start._1).abs
    val yStep = if (end._2 - start._2 == 0) 0 else (end._2 - start._2) / (end._2 - start._2).abs

    @tailrec
    def calculateCoordinates(currentCoordinate: (Int, Int), coordinates: List[(Int, Int)] = Nil): List[(Int, Int)] =
      currentCoordinate match {
        case c if c == end => coordinates :+ end
        case _ => calculateCoordinates((currentCoordinate._1 + xStep, currentCoordinate._2 + yStep), coordinates :+ currentCoordinate)
      }

    calculateCoordinates(start)
  }
}

object HydrothermalVenture extends App {
  def countOverlaps(lines: List[Line]): Int =
    lines
      .flatMap(_.coordinatesCovered)
      .groupBy(identity)
      .count(coordinates => coordinates._2.size >= 2)


  val linePattern: Regex = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r
  val lines = FileReader.readInput("y2021/day5").map {
    case linePattern(a, b, x, y) => Line((a.toInt, b.toInt), (x.toInt, y.toInt))
  }
  val nonDiagonalLines = lines.filter(line => line.start._1 == line.end._1 || line.start._2 == line.end._2)

  println(countOverlaps(nonDiagonalLines))
  println(countOverlaps(lines))
}

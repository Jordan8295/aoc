package y2021.day2

import utils.FileReader

import scala.util.matching.Regex

sealed trait Direction {
  val magnitude: Int
  def amendPosition(position: (Int, Int)): (Int, Int)
  def amendPositionWithAim(position: (Int, Int, Int)): (Int, Int, Int)
}

case class Forward(magnitude: Int) extends Direction {
  def amendPosition(position: (Int, Int)): (Int, Int) = (position._1 + magnitude, position._2)

  def amendPositionWithAim(position: (Int, Int, Int)): (Int, Int, Int) =
    (position._1 + magnitude, position._2 + (position._3 * magnitude), position._3)
}

object Forward {
  val pattern: Regex = "(forward) (\\d+)".r
}

case class Down(magnitude: Int) extends Direction {
  def amendPosition(position: (Int, Int)): (Int, Int) = (position._1, position._2 + magnitude)

  def amendPositionWithAim(position: (Int, Int, Int)): (Int, Int, Int) = (position._1, position._2, position._3 + magnitude)
}

object Down {
  val pattern: Regex = "(down) (\\d+)".r
}

case class Up(magnitude: Int) extends Direction {
  def amendPosition(position: (Int, Int)): (Int, Int) = (position._1, position._2 - magnitude)

  def amendPositionWithAim(position: (Int, Int, Int)): (Int, Int, Int) = (position._1, position._2, position._3 - magnitude)
}

object Up {
  val pattern: Regex = "(up) (\\d+)".r
}

object Dive extends App {
  def calculatePosition(input: List[Direction]): (Int, Int) = input.foldLeft((0, 0)) {
    (position, direction) =>
      direction.amendPosition(position)
  }

  def calculatePositionWithAim(input: List[Direction]): (Int, Int, Int) = input.foldLeft((0, 0, 0)) {
    (position, direction) =>
      direction.amendPositionWithAim(position)
  }

  val input: List[Direction] = FileReader.readInput("y2021/day2")
    .map {
      case Forward.pattern(_, magnitude) => Forward(magnitude.toInt)
      case Down.pattern(_, magnitude) => Down(magnitude.toInt)
      case Up.pattern(_, magnitude) => Up(magnitude.toInt)
    }

  println(calculatePosition(input)._1 * calculatePosition(input)._2)
  println(calculatePositionWithAim(input)._1 * calculatePositionWithAim(input)._2)
}

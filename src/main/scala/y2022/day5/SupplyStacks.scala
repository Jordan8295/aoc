package y2022.day5

import utils.FileReader

import scala.util.matching.Regex

case class Move(amount: Int, from: Int, to: Int)

object SupplyStacks extends App {
  def moveCratesCrateMover9000(stacks: List[List[Char]], moves: List[Move]): List[Char] = moves.foldLeft(stacks) {
    (stacks, move) =>
      stacks
        .updated[List[Char]](move.to - 1, stacks(move.to - 1) ::: stacks(move.from - 1).takeRight(move.amount).reverse)
        .updated[List[Char]](move.from - 1, stacks(move.from - 1).dropRight(move.amount))
  }.filter(_.nonEmpty).map(_.last)

  def moveCratesCrateMover9001(stacks: List[List[Char]], moves: List[Move]): List[Char] = moves.foldLeft(stacks) {
    (stacks, move) =>
      stacks
        .updated[List[Char]](move.to - 1, stacks(move.to - 1) ::: stacks(move.from - 1).takeRight(move.amount))
        .updated[List[Char]](move.from - 1, stacks(move.from - 1).dropRight(move.amount))
  }.filter(_.nonEmpty).map(_.last)

  val input: List[String] = FileReader.readInput("y2022/day5")

  val stackPattern: Regex = "(\\[[A-Z]]| {3})? ?(\\[[A-Z]]| {3})? ?(\\[[A-Z]]| {3})? ?(\\[[A-Z]]| {3})? ?(\\[[A-Z]]| {3})? ?(\\[[A-Z]]| {3})? ?(\\[[A-Z]]| {3})? ?(\\[[A-Z]]| {3})? ?(\\[[A-Z]]| {3})?".r
  val sanitiseString: String => Option[String] = s => Option(s.replaceAll("[\\[\\]]", "").strip()).filterNot(_ == "")
  val stacks: List[List[Char]] = input.filter {
    case stackPattern(_, _, _, _, _, _, _, _, _) => true
    case _ => false
  }.map {
    case stackPattern(a, b, c, d, e, f, g, h, i) => List(
      Option(a).flatMap(sanitiseString),
      Option(b).flatMap(sanitiseString),
      Option(c).flatMap(sanitiseString),
      Option(d).flatMap(sanitiseString),
      Option(e).flatMap(sanitiseString),
      Option(f).flatMap(sanitiseString),
      Option(g).flatMap(sanitiseString),
      Option(h).flatMap(sanitiseString),
      Option(i).flatMap(sanitiseString)
    )
  }.transpose.map(_.take(8).reverse.flatten.map(_.head))

  val movePattern: Regex = "move (\\d+) from (\\d+) to (\\d+)".r
  val moves: List[Move] = input.filter {
    case movePattern(_, _, _) => true
    case _ => false
  }.map {
    case movePattern(amount, from, to) => Move(amount.toInt, from.toInt, to.toInt)
  }

  println(moveCratesCrateMover9000(stacks, moves).mkString)
  println(moveCratesCrateMover9001(stacks, moves).mkString)
}

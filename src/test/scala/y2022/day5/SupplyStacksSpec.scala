package y2022.day5

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SupplyStacksSpec extends AnyWordSpec with should.Matchers {
  "Moving crates with CrateMover 9000" should {
    "return item at the top when 1 crate is moved to an empty stack" in {
      val stacks = List(List('A'), Nil)
      val moves = List(Move(1, 1, 2))
      SupplyStacks.moveCratesCrateMover9000(stacks, moves) should be(List('A'))
    }

    "return item at the top when 2 crates are moved to an empty stack" in {
      val stacks = List(List('A', 'B'), Nil)
      val moves = List(Move(2, 1, 2))
      SupplyStacks.moveCratesCrateMover9000(stacks, moves) should be(List('A'))
    }

    "return items at the top when crates are moved between multiple stacks" in {
      val stacks = List(List('Z', 'N'), List('M', 'C', 'D'), List('P'))
      val moves = List(Move(1, 2, 1), Move(3, 1, 3), Move(2, 2, 1), Move(1, 1, 2))
      SupplyStacks.moveCratesCrateMover9000(stacks, moves) should be(List('C', 'M', 'Z'))
    }
  }

  "Moving crates with CrateMover 9001" should {
    "return item at the top when 1 crate is moved to an empty stack" in {
      val stacks = List(List('A'), Nil)
      val moves = List(Move(1, 1, 2))
      SupplyStacks.moveCratesCrateMover9001(stacks, moves) should be(List('A'))
    }

    "return item at the top when 2 crates are moved to an empty stack" in {
      val stacks = List(List('A', 'B'), Nil)
      val moves = List(Move(2, 1, 2))
      SupplyStacks.moveCratesCrateMover9001(stacks, moves) should be(List('B'))
    }

    "return items at the top when crates are moved between multiple stacks" in {
      val stacks = List(List('Z', 'N'), List('M', 'C', 'D'), List('P'))
      val moves = List(Move(1, 2, 1), Move(3, 1, 3), Move(2, 2, 1), Move(1, 1, 2))
      SupplyStacks.moveCratesCrateMover9001(stacks, moves) should be(List('M', 'C', 'D'))
    }
  }
}

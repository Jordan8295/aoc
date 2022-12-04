package y2022.day3

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import y2022.day3.RucksackReorganisation

class RucksackReorganisationSpec extends AnyWordSpec with should.Matchers {
  "Finding errors" should {
    "calculate the priority of a bag with two items" in {
      val input = List(("t", "t"))
      RucksackReorganisation.findErrors(input) should be(List('t'))
    }

    "calculate the priority of a bag with multiple items" in {
      val input = List(("PPoITUTwuL", "ZmTXbvmZxcn"))
      RucksackReorganisation.findErrors(input) should be(List('T'))
    }

    "calculate the priority of multiple bags" in {
      val input = List(("t", "t"), ("T", "T"), ("vXy", "Pvp"))
      RucksackReorganisation.findErrors(input) should be(List('t', 'T', 'v'))
    }
  }

  "Finding badges" should {
    "find common items in a single group with bags of single items" in {
      val input = List(List("c", "c", "c"))
      RucksackReorganisation.findBadges(input) should be(List('c'))
    }

    "find common items in a single group with bags of multiple items" in {
      val input = List(List("qwueoqwcIOiwerue", "cAdasjkfghksl", "ZcnmzxBmxbBxznxzmccN"))
      RucksackReorganisation.findBadges(input) should be(List('c'))
    }

    "find common items in multiple groups" in {
      val input = List(List("qwueoqwcIOiwerue", "cAdasjkfghksl", "ZcnmzxBmxbBxznxzmccN"), List("L", "L", "L"))
      RucksackReorganisation.findBadges(input) should be(List('c', 'L'))
    }
  }

  "Calculating priority" should {
    "return priority of single lower case characters" in {
      val input = List('t')
      RucksackReorganisation.getPriority(input) should be(20)
    }

    "return priority of single upper case characters" in {
      val input = List('T')
      RucksackReorganisation.getPriority(input) should be(46)
    }

    "return priority of multiple characters" in {
      val input = List('t', 'T', 'v')
      RucksackReorganisation.getPriority(input) should be(88)
    }
  }
}

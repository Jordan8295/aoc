package y2022.day1

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers._
import y2022.day1.CalorieCounting

class CalorieCountingSpec extends AnyWordSpec with should.Matchers {
  "The max calorie count" should {
    "count the calories in a single group" in {
      val input = List(List(300, 200, 5))
      CalorieCounting.maxCalories(input) should be(505)
    }

    "count the calories in multiple groups with a single item in each" in {
      val input = List(List(300), List(200), List(5))
      CalorieCounting.maxCalories(input) should be(300)
    }

    "count the calories in multiple groups with multiple items in each" in {
      val input = List(List(300, 600, 2), List(200, 900, 7), List(5, 6, 9, 4), List(1000))
      CalorieCounting.maxCalories(input) should be(1107)
    }
  }

  "The top 3 calorie count" should {
    "count the calories carried by top 3 elves when 3 elves are provided with a single item each" in {
      val input = List(List(1), List(300), List(200))
      CalorieCounting.top3MaxCalories(input) should be(501)
    }

    "count the calories carried by top 3 elves when 3 elves are provided with a multiple items each" in {
      val input = List(List(1, 500), List(300, 900, 2), List(200, 700, 100, 100))
      CalorieCounting.top3MaxCalories(input) should be(2803)
    }

    "count the calories carried by top 3 elves when more than 3 elves are provided with a multiple items each" in {
      val input = List(List(1, 500), List(200, 100), List(300, 900, 2), List(1), List(200, 700, 100, 100))
      CalorieCounting.top3MaxCalories(input) should be(2803)
    }
  }
}

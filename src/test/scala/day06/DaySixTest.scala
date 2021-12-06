package day06

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper


class DaySixTest extends AnyFunSpec {

  describe("DaySix") {
    it("should should update state") {

      val state = Seq(3,4,3,1,2)

      val day1 = DaySix.nextState(state)
      day1 shouldBe Seq(2,3,2,0,1)

      val day2 = DaySix.nextState(day1)
      day2 shouldBe Seq(1,2,1,6,0,8)

      val day3 = DaySix.nextState(day2)
      day3 shouldBe Seq(0,1,0,5,6,7,8)

      val day4 = DaySix.nextState(day3)
      day4 shouldBe Seq(6,0,6,4,5,6,7,8,8)

      val day5 = DaySix.nextState(day4)
      day5 shouldBe Seq(5,6,5,3,4,5,6,7,7,8)

    }
  }
}

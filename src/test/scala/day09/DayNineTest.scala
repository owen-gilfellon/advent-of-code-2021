package day09

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DayNineTest extends AnyFunSpec {

  describe("DayNine") {

    val inputLines = Seq("2199943210",
      "3987894921",
      "9856789892",
      "8767896789",
      "9899965678")

    val heights = inputLines.map(_.map(_.toString.toInt))

    it("should calculate lowest points correctly") {

      DayNine.getLowestPoints(heights).map(_._1) shouldBe Seq(1, 0, 5, 5)

    }

    it("should calculate basin sizes correctly") {

      DayNine.getLowestPoints(heights).map(z => DayNine.getBasin(heights, z._2))
        .map(_.size).sorted shouldBe Seq(3, 9, 9, 14)

    }
  }

}

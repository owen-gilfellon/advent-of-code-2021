package day07

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper


class DaySevenTest extends AnyFunSpec {

  describe("DaySeven") {
    it("should calculate fuel costs correctly") {

      val initialPositions = Seq(16,1,2,0,4,2,7,1,2,14)

      DaySeven.getLinearFuelCost(initialPositions, 1) shouldBe 41
      DaySeven.getLinearFuelCost(initialPositions, 2) shouldBe 37
      DaySeven.getLinearFuelCost(initialPositions, 3) shouldBe 39
      DaySeven.getLinearFuelCost(initialPositions, 10) shouldBe 71
      DaySeven.findLowestFuelCost(DaySeven.getLinearFuelCost)(initialPositions, 0 to initialPositions.max) shouldBe 2

      DaySeven.getTriangleNumberFuelCost(initialPositions, 2) shouldBe 206
      DaySeven.getTriangleNumberFuelCost(initialPositions, 5) shouldBe 168
      DaySeven.findLowestFuelCost(DaySeven.getTriangleNumberFuelCost)(initialPositions, 0 to initialPositions.max) shouldBe 5
    }
  }
}

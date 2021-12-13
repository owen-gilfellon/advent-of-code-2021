package day11

import day11.DayEleven.{Grid, NeigbourhoodIndex, OctoGrid, octopusFlashes}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class DayElevenTest extends AnyFunSpec {


  describe("neighbourhoodIndex") {

    val gridSizeX = 10

    it("Eight Neighbour strategy should select correct indexes") {

      val neighbourStrategy =  NeigbourhoodIndex(gridSizeX, gridSizeX)

      neighbourStrategy.areOnSameLine(0, -2) shouldBe false
      neighbourStrategy.areOnSameLine(0, -1) shouldBe false
      neighbourStrategy.areOnSameLine(0, 1) shouldBe true
      neighbourStrategy.areOnSameLine(0, 9) shouldBe true
      neighbourStrategy.areOnSameLine(9, 0) shouldBe true
      neighbourStrategy.areOnSameLine(9, 10) shouldBe false
      neighbourStrategy.areOnSameLine(10, 9) shouldBe  false
      neighbourStrategy.areOnSameLine(0, 10) shouldBe false
      neighbourStrategy.areOnSameLine(10, 0) shouldBe false

      neighbourStrategy.eightNeighbourStrategy(0) shouldBe Seq(1, gridSizeX, gridSizeX + 1)
      neighbourStrategy.eightNeighbourStrategy(9) shouldBe Seq(8, 8 + gridSizeX, 9 + gridSizeX)
      neighbourStrategy.eightNeighbourStrategy(11) shouldBe Seq(0, 1, 2, 10, 12, 20, 21, 22)
      neighbourStrategy.eightNeighbourStrategy(99) shouldBe Seq(98 - gridSizeX, 99 - gridSizeX, 98)

    }

    it("should handle  middles") {

      NeigbourhoodIndex(gridSizeX, gridSizeX).eightNeighbourStrategy(55) shouldBe Seq(
        54 - gridSizeX, 55 -gridSizeX, 56 - gridSizeX,
        54,  56,
        54 + gridSizeX, 55 + gridSizeX, 56 + gridSizeX )
    }

  }

  describe("octopusFlashes") {

    it("should step forward a 5x5 grid correctly") {

      val small1 = "11111\n19991\n19191\n19991\n11111"
      val small2 = "34543\n40004\n50005\n40004\n34543"
      val small3 = "45654\n51115\n61116\n51115\n45654"

      val small1Grid = OctoGrid(small1, 5)
      val small2Grid = OctoGrid(small2, 5)
      val small3Grid = OctoGrid(small3, 5)

      val applyFlashes = octopusFlashes(NeigbourhoodIndex(5,5).eightNeighbourStrategy) _

      val smallStep2 = applyFlashes(small1Grid)

      smallStep2 shouldBe small2Grid

      val smallStep3 = applyFlashes(smallStep2)

      smallStep3 shouldBe small3Grid
    }

    it("should step forward a 10 x 10 grid correctly") {

      val large1 = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n" +
                    "4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

      val large2 = "6594254334\n3856965822\n6375667284\n7252447257\n7468496589\n" +
                    "5278635756\n3287952832\n7993992245\n5957959665\n6394862637"

      val large3 = "8807476555\n5089087054\n8597889608\n8485769600\n8700908800\n" +
                    "6600088989\n6800005943\n0000007456\n9000000876\n8700006848"

      val large4 = "0050900866\n8500800575\n9900000039\n9700000041\n9935080063\n" +
                    "7712300000\n7911250009\n2211130000\n0421125000\n0021119000"

      val large5 = "2263031977\n0923031697\n0032221150\n0041111163\n0076191174\n" +
                    "0053411122\n0042361120\n5532241122\n1532247211\n1132230211"

      val applyFlashes = octopusFlashes(NeigbourhoodIndex(10,10).eightNeighbourStrategy) _

      val largeStep2 = applyFlashes(OctoGrid(large1, 10))

      largeStep2 shouldBe OctoGrid(large2, 10)

      val largeStep3 = applyFlashes(OctoGrid(large2, 10))

      largeStep3 shouldBe OctoGrid(large3, 10)

      val largeStep4 = applyFlashes(OctoGrid(large3, 10))

      largeStep4 shouldBe OctoGrid(large4, 10)

      val largeStep5 = applyFlashes(OctoGrid(large4, 10))

      largeStep5 shouldBe OctoGrid(large5, 10)


    }
  }

}

package day07

import scala.io.Source

object DaySeven {

  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/day07/input.txt"
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val crabPositions = inputLines.flatMap(_.split(",")).map(_.toInt)

    val lowestCostTarget = findLowestFuelCost(getLinearFuelCost)(crabPositions, 0 to crabPositions.max)
    println(s"Lowest cost is position $lowestCostTarget, at ${getLinearFuelCost(crabPositions, lowestCostTarget)} fuel units")

    val lowestTriangleCostTarget = findLowestFuelCost(getTriangleNumberFuelCost)(crabPositions, 0 to crabPositions.max)
    println(s"Lowest triangle number cost is position $lowestTriangleCostTarget, at ${getTriangleNumberFuelCost(crabPositions, lowestTriangleCostTarget)} fuel units")
  }

  // i.e. 1 + 2 + 3 + 4 .... + x
  def nthTriangleNumber(x: Int): BigInt = {
    (BigInt(x) * BigInt(x + 1)) / BigInt(2)
  }

  def getLinearFuelCost(initialPositions: Seq[Int], targetPosition: Int): BigInt =
    initialPositions.map(pos => Math.abs(pos - targetPosition)).sum

  def getTriangleNumberFuelCost(initialPositions: Seq[Int], targetPosition: Int): BigInt =
    initialPositions.map(pos => nthTriangleNumber(Math.abs(pos - targetPosition))).sum

  def findLowestFuelCost(fuelCalculator: (Seq[Int], Int) => BigInt)(positions: Seq[Int], targetPositions: Seq[Int]): Int =
    targetPositions.distinct.map(pos => (pos, fuelCalculator(positions, pos))).minBy(_._2)._1
}
package day09

import scala.annotation.tailrec
import scala.io.Source

object DayNine {

  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/day09/input.txt"
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val heights: Seq[Seq[Int]] = inputLines.map(_.map(_.toString.toInt))

    println(s"Sum of Lowest Points = ${getLowestPoints(heights).map(_._1).map(x => x + 1).sum}")

    val basinSizes = DayNine.getLowestPoints(heights).map(lowPoint => DayNine.getBasin(heights, lowPoint._2).size)

    println(s"Product of three biggest basins = ${basinSizes.sorted.reverse.take(3).product}")
  }

  def getBasin(heights: Seq[Seq[Int]], lowPoint: Int):Seq[Int] = {
    getBasin(heights.head.size, heights.flatten, Seq(lowPoint), Seq(lowPoint))
  }

  @tailrec
  def getBasin(lineLength:Int, heights: Seq[Int], toSearch: Seq[Int], basin:Seq[Int]): Seq[Int] = {
    if(toSearch.isEmpty)
      basin
    else {
      val neighbourhoodIndexes = getNeighbourhood(lineLength, heights, toSearch.head).filter(a => a._1 != 9).map(_._2)
      getBasin(
        lineLength = lineLength,
        heights = heights,
        toSearch = (toSearch.tail ++ neighbourhoodIndexes.filterNot(x => basin.contains(x))).distinct,
        basin = (basin ++ neighbourhoodIndexes).distinct)
    }
  }

  def getLowestPoints(heights: Seq[Seq[Int]]): Seq[(Int, Int)] = {
    val flattenedHeights = heights.flatten
    flattenedHeights.zipWithIndex.filter(x => isLowestPoint(heights.head.size, flattenedHeights, x._2))
  }

  def isLowestPoint(lineLength: Int, heights: Seq[Int], x: Int): Boolean = {
    val currentHeight = heights(x)
    getNeighbourhood(lineLength, heights, x).filter(_._2 != x).map(_._1)
      .minOption.exists(y => y > currentHeight)
  }

  def getNeighbourhood(lineLength:Int, heights:Seq[Int], x: Int): Seq[(Int, Int)] = {
    getNeighbourhoodIndexes(lineLength, heights.length, x).map(index => (heights(index), index))
  }

  def getNeighbourhoodIndexes(lineLength:Int, listLength:Int, x:Int): Seq[Int] =
    (Seq(x - lineLength, x + lineLength) ++ Seq(x - 1, x, x + 1).filter(y => areOnSameLine(x, y, lineLength)))
      .filter(y => y >= 0 && y < listLength)

  def areOnSameLine(x:Int, y:Int, lineLength:Int): Boolean =
    x / lineLength == y / lineLength

}

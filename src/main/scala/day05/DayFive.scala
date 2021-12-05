package day05

import scala.io.Source

object DayFive {

  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/day05/input.txt"
    val linePattern = "([\\d]+),([\\d]+) -> ([\\d]+),([\\d]+)".r
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val lines: Seq[Line] = inputLines.filter(_.matches(linePattern.regex))
      .flatMap(line => linePattern.findFirstMatchIn(line).map(data =>
        Line(
          Coordinate(data.group(1).toInt, data.group(2).toInt),
          Coordinate(data.group(3).toInt, data.group(4).toInt))))

    println(s"Crossing in ${countCrossings(lines, line => line.isVertical || line.isHorizontal)} places")
    println(s"Crossing in ${countCrossings(lines, line => line.isVertical || line.isHorizontal || line.isDiagonal)} places")

  }

  def countCrossings(lines: Seq[Line], filter: Line => Boolean): Int =
    lines.filter(filter)
      .flatMap(_.rasteriseLine)
      .groupBy(identity).view
      .mapValues(_.size).toMap.toList
      .count(point => point._2 >= 2)

  case class Line(from: Coordinate, to: Coordinate) {

    def isVertical: Boolean = from.x == to.x
    def isHorizontal: Boolean = from.y == to.y
    def isDiagonal: Boolean = Math.abs(from.y - to.y) == Math.abs(from.x - to.x)

    def rasteriseLine: Seq[Coordinate] = {
        val xRange = if(from.x == to.x) Seq.fill(Math.abs(from.y - to.y))(from.x)
          else if(from.x <= to.x) from.x to to.x else (to.x to from.x).reverse
        val yRange = if(from.y == to.y) Seq.fill(Math.abs(from.x - to.x))(from.y)
          else if(from.y <= to.y) from.y to to.y else (to.y to from.y).reverse
        xRange.zip(yRange).map(xy => Coordinate(xy._1, xy._2))
    }
  }

  case class Coordinate(x: Int, y:Int)
}

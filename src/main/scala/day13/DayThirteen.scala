package day13

import scala.io.Source

object DayThirteen {

  def main(args: Array[String]): Unit = {
    val filename = "src/main/resources/day13/input.txt"
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val coordinateLinePattern = "([\\d]+),([\\d]+)".r
    val foldLinePattern = "fold along ([xy])=([\\d]+)".r

    val inputCoordinates: Seq[Coordinate] = inputLines.flatMap(s => coordinateLinePattern.findFirstMatchIn(s).map(m => Coordinate(m.group(1).toInt, m.group(2).toInt)))
    val inputFolds: Seq[Fold] = inputLines.flatMap(s => foldLinePattern.findFirstMatchIn(s).map(m => Fold(m.group(1).equals("x"), m.group(2).toInt)))

    println(inputCoordinates.map(c => c.apply(inputFolds.head)).distinct.size)

    val updated: Seq[Coordinate] = inputFolds.foldLeft(inputCoordinates)((x, y) => x.map(c => c.apply(y)).distinct)
    println(printGrid(updated))
  }

  def printGrid(coordinates: Seq[Coordinate]): String = {
    (0 to coordinates.map(_.y).max).map(line =>
      (0 to coordinates.map(_.x).max).map(column =>
        if(coordinates.exists(coordinate => coordinate.x == column && coordinate.y == line)) "#" else ".").mkString).mkString("\n")
  }

  case class Fold(alongX: Boolean, line: Int)

  case class Coordinate(x: Int, y: Int) {
    def apply(fold: Fold): Coordinate = {
      val newX = if(fold.alongX) mirror(x, fold.line) else x
      val newY = if(!fold.alongX) mirror(y, fold.line) else y
      Coordinate(newX,  newY)
    }

    def mirror(original: Int, mirrorPoint: Int): Int = {
      if(original < mirrorPoint) original else mirrorPoint - (original - mirrorPoint)
    }
  }
}

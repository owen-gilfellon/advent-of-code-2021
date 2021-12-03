package day01

import scala.annotation.tailrec
import scala.io.Source

object DayOne {

  var windowSize = 3

  def main(args: Array[String]): Unit = {
    val filename = "src/main/resources/day01/input.txt"
    val reader = Source.fromFile(filename)
    val lines: Seq[Int] = reader.getLines.toSeq.map(_.toInt)
    reader.close()
    println(countIncreases(lines, 0))
  }

  @tailrec
  def countIncreases(readings: Seq[Int], count: Int): Int = {
    if(readings.size <= windowSize ) {
      count
    } else {
      val tail = readings.tail
      val firstWindow = readings.take(windowSize).sum
      val secondWindow = tail.take(windowSize).sum
      countIncreases(tail, if(firstWindow < secondWindow) count + 1 else count)
    }
  }
}

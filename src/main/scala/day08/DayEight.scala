package day08

import scala.annotation.tailrec
import scala.io.Source

object DayEight {

  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/day08/input.txt"
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val signalPatterns = inputLines.map(_.split("\\|")(0)).map(_.split(" ").toSeq.filter(_.nonEmpty))
    val outputs = inputLines.map(_.split("\\|")(1)).map(_.split(" ").toSeq.filter(_.nonEmpty))

    println(s"Count of all 1, 4, 7 and 8s = ${outputs.flatten.count(s => sizesOfKnownDigits.contains(s.length))}")
    println(s"Sum of all outputs = ${signalPatterns.zip(outputs).map(x  => processLine(x._1, x._2)).sum}")
  }

  val sizesOfKnownDigits = Seq(2, 3, 4, 7)

  def processLine(inputs:Seq[String], outputs:Seq[String]): Int = {
    val mappings = calculateMappings(inputs)
    outputs.flatMap(s => mappings.find(_._2 == s.toSet).map(_._1)).mkString.toInt
  }

  def calculateMappings(inputs: Seq[String]): Map[Int, Set[Char]] = {

    val knownAndUnknownInputs = inputs.partition(i => sizesOfKnownDigits.contains(i.length))

    val knownMappings = knownAndUnknownInputs._1.map(i =>
      if(i.length == 2)
        (1, i.toSet)
      else if(i.length == 3)
        (7, i.toSet)
      else if(i.length == 4)
        (4, i.toSet)
      else
        (8, i.toSet)).toMap

    calculateUnknownMappings(knownAndUnknownInputs._2, knownMappings)
  }

  @tailrec
  def calculateUnknownMappings(inputs: Seq[String], mappings: Map[Int, Set[Char]]): Map[Int, Set[Char]] = {
    if(inputs.isEmpty)
      mappings
    else{
      val input = inputs.head.toSet
      val diffs = (input.size, input.diff(mappings(1)).size, input.diff(mappings(4)).size)
      val mapping = diffs match {
        case (5, 4, 3) => (2, input)
        case (5, 3, _) => (3, input)
        case (5, 4, 2) => (5, input)
        case (6, 4, 3) => (0, input)
        case (6, 5, _) => (6, input)
        case (6, 4, 2) => (9, input)
      }
      calculateUnknownMappings(inputs.tail, mappings + mapping)
    }
  }
}

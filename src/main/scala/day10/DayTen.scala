package day10

import scala.annotation.tailrec
import scala.io.Source

object DayTen {

  def main(args: Array[String]): Unit = {
    val filename = "src/main/resources/day10/input.txt"
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    println(s"Error score = ${inputLines.flatMap(line => getErrorScore(line)).sum}")
    println(s"Autocomplete score = ${getMiddleValue(inputLines.flatMap(getAutocompleteScore))}")
    reader.close()
  }

  val symbols = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>')

  val errorScores = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val autocompleteScores = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  def getMiddleValue(values: Seq[BigInt]): Option[BigInt] = {
    if(values.isEmpty)
      None
    else Some(values.sorted.apply(values.size / 2))
  }

  def getErrorScore(line: String): Option[Int] = {
    parseLine(line, Seq()) match {
      case Right(value) => Some(errorScores(value))
      case Left(_) => None
    }
  }

  def getAutocompleteScore(line: String): Option[BigInt] = {
    parseLine(line, Seq()) match {
      case Right(_) => None
      case Left(string) => Some(string.flatMap(autocompleteScores.get).map(BigInt.apply)
      .fold(BigInt(0))((a, b) => a * 5 + b))
    }
  }

  @tailrec
  def autocompleteString(openings: Seq[Char], closings: Seq[Char]): Seq[Char] = {
    if(openings.isEmpty)
      closings
    else
      autocompleteString(openings.dropRight(1), closings :+ symbols(openings.last))
  }

  @tailrec
  def parseLine(string: String, openings: Seq[Char]): Either[Seq[Char], Char] = {
    if(string.isEmpty) {
      Left(autocompleteString(openings, Seq()))
    } else if(symbols.keySet.contains(string.head)) {
      parseLine(string.tail, openings :+ string.head)
    } else if(symbols.values.toSet.contains(string.head)) {
      if(symbols(openings.last) != string.head) {
        Right(string.head)
      } else {
        parseLine(string.tail, openings.dropRight(1))
      }
    } else {
      throw new IllegalArgumentException(s"${string.head} is not a valid symbol")
    }
  }
}

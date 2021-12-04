package day03

import scala.annotation.tailrec
import scala.io.Source

object DayThree {

  val lineLength = 12

  def mostCommonDigit(digits:Seq[Int]): Int = if(digits.sum >= (digits.size.toDouble / 2.0)) 1 else 0

  def leastCommonDigit(digits:Seq[Int]): Int = if(digits.sum >= (digits.size.toDouble / 2.0)) 0 else 1

  def transposeBinaryLines(lineLength: Int, lines:Seq[Seq[Int]]): Seq[Seq[Int]] = Seq.range(0, lineLength).map(index => lines.map(indexedSeq => indexedSeq(index)))

  @tailrec
  def reduceDigits(binaryLines: Seq[Seq[Int]], position: Int, reducer: Seq[Int] => Int): Seq[Seq[Int]] = {
    if(binaryLines.size == 1 || position == binaryLines.head.size)
      return binaryLines

    val transposedBinaryLines = transposeBinaryLines(lineLength, binaryLines)
    val filteringBit = reducer(transposedBinaryLines(position))
    val filteredBinaryLines = binaryLines.filter(line => line(position) == filteringBit)

    reduceDigits(filteredBinaryLines, position + 1, reducer)
  }

  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/day03/input.txt"
    val reader = Source.fromFile(filename)
    val reportLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val binaryLines: Seq[IndexedSeq[Int]] = reportLines.map(s => s.map(_.toString.toInt))
    val transposedBinaryLines: Seq[Seq[Int]] = transposeBinaryLines(lineLength, binaryLines)

    val gamma = transposedBinaryLines.map(mostCommonDigit).foldLeft("")((a, b) => a + b)
    val epsilon = transposedBinaryLines.map(leastCommonDigit).foldLeft("")((a, b) => a + b)

    val gammaInt = Integer.parseInt(gamma,2)
    val epsilonInt = Integer.parseInt(epsilon,2)

    println(s"gamma = $gamma => $gammaInt")
    println(s"epsilon = $epsilon => $epsilonInt")
    println(s"gamma x epsilon = ${gammaInt * epsilonInt}")

    val oxygenRating = reduceDigits(binaryLines, 0, mostCommonDigit).head.foldLeft("")((a, b) => a + b)
    val co2Rating = reduceDigits(binaryLines, 0, leastCommonDigit).head.foldLeft("")((a, b) => a + b)

    val oxygenInt = Integer.parseInt(oxygenRating,2)
    val co2Int = Integer.parseInt(co2Rating,2)

    println(s"o2 = $oxygenRating => $oxygenInt")
    println(s"co2 = $co2Rating => $co2Int")
    println(s"o2 x co2 = ${oxygenInt * co2Int}")
  }
}
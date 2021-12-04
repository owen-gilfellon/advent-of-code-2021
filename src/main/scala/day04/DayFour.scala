package day04

import scala.annotation.tailrec
import scala.io.Source

object DayFour {

  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/day04/input.txt"
    val bingoCardLinePattern = "([ ]*([\\d]+)){5}"
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val numbersToCall:Seq[Int] = inputLines.head.split(",").map(_.toInt)
    val bingoBoards = inputLines.tail.filter(_.matches(bingoCardLinePattern))
      .map(_.split(" ").filterNot(_.isEmpty).map(_.toInt).toSeq).grouped(5)
      .map(BingoBoard.apply).toSeq

    println(s"parsed ${bingoBoards.size} boards & ${numbersToCall.size} numbers to call")

    val orderedWinningBoards = getOrderedWinners(bingoBoards, numbersToCall)

    println(s"First winning score: ${orderedWinningBoards.headOption.map(_.score)}")
    println(s"Last winning score: ${orderedWinningBoards.lastOption.map(_.score)}")
  }

  def getOrderedWinners(bingoBoards: Seq[BingoBoard], numbersToCall: Seq[Int]): Seq[WinningBoard] = {
    getOrderedWinners(bingoBoards, Seq(), Seq(numbersToCall.head), numbersToCall.tail)
  }

  @tailrec
  def getOrderedWinners( bingoBoards: Seq[BingoBoard],
                         orderedWinners: Seq[WinningBoard],
                         numbersCalled: Seq[Int],
                         numbersToCall: Seq[Int]): Seq[WinningBoard] = {
    val winningBoards = bingoBoards.filter(_.isComplete(numbersCalled))
    if (numbersToCall.isEmpty)
      orderedWinners
    else if (winningBoards.nonEmpty)
      getOrderedWinners(
        bingoBoards.diff(winningBoards),
        orderedWinners ++ winningBoards
          .map(board => WinningBoard(board, board.getScore(numbersCalled))),
        numbersCalled :+ numbersToCall.head,
        numbersToCall.tail)
    else
      getOrderedWinners(
        bingoBoards,
        orderedWinners,
        numbersCalled :+ numbersToCall.head,
        numbersToCall.tail)
  }

  case class WinningBoard(bingoBoard: BingoBoard, score: Int)

  case class BingoBoard(matrix: Seq[Seq[Int]]) {

    private def transpose(matrix:Seq[Seq[Int]]): Seq[Seq[Int]] =
      if(matrix.isEmpty || matrix.head.isEmpty)
        matrix
      else
        transpose(matrix.head.size, matrix)

    private def transpose(lineLength: Int, matrix:Seq[Seq[Int]]): Seq[Seq[Int]] =
      Seq.range(0, lineLength).map(index => matrix.map(indexedSeq => indexedSeq(index)))

    private def hasCompleteRow(rows: Seq[Seq[Int]], calledNumbers: Seq[Int]): Boolean =
      rows.exists(_.diff(calledNumbers).isEmpty)

    def isComplete(calledNumbers: Seq[Int]): Boolean =
      hasCompleteRow(matrix, calledNumbers) || hasCompleteRow(transpose(matrix), calledNumbers)

    def getScore(calledNumbers: Seq[Int]): Int =
      matrix.flatMap(_.diff(calledNumbers)).sum * calledNumbers.last
  }
}

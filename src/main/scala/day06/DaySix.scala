package day06

import scala.annotation.tailrec
import scala.io.Source

object DaySix {

  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/day06/input.txt"
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val firstState = inputLines.flatMap(_.split(",")).map(_.toInt)

    println(iterateStates(80, firstState).size)
    println(iterateStates(256, getOptimisedState(firstState)).values.sum)
  }

  def nextState(state: Seq[Int]): Seq[Int] = {
    state.map(fish => {
      if(fish == 0)
        6
      else
        fish - 1
    }) ++ state.filter(_ == 0).map(_ => 8)
  }

  def getOptimisedState(state: Seq[Int]): Map[Int, BigInt] = {
    state.groupBy(identity).view.mapValues(v => BigInt(v.size)).toMap
  }

  // Optimised method of updating states
  def nextState(state: Map[Int, BigInt]): Map[Int, BigInt] = {
    (0 to 8).map(index => {
      if(index == 6)
        (index, state.getOrElse(7,BigInt(0)) + state.getOrElse(0, BigInt(0)))
      else if(index == 8)
        (index, state.getOrElse(0,BigInt(0)))
      else
        (index, state.getOrElse(index + 1, BigInt(0)))
    }).toMap
  }

  @tailrec
  def iterateStates(iterations: Int, state: Seq[Int]): Seq[Int] = {
    if(iterations == 0)
      state
    else
      iterateStates(iterations - 1, nextState(state))
  }

  @tailrec
  def iterateStates(iterations: Int, state: Map[Int, BigInt]): Map[Int, BigInt] = {
    if(iterations == 0)
      state
    else
      iterateStates(iterations - 1, nextState(state))
  }
}

package day02

import day02.DayTwo.Direction.{Down, Forward, Up}

import scala.io.Source

object DayTwo {

  def main(args: Array[String]): Unit = {
    val filename = "src/main/resources/day02/input.txt"
    val reader = Source.fromFile(filename)
    val commands: Seq[String] = reader.getLines.toSeq
    reader.close()

    def strategyOne(state:SubmarineState, command: SubmarineCommand): SubmarineState = {
      command match {
        case SubmarineCommand(Forward, x) => SubmarineState(state.horizontal + x, state.depth, state.aim)
        case SubmarineCommand(Down, x) => SubmarineState(state.horizontal, state.depth + x, state.aim)
        case SubmarineCommand(Up, x) => SubmarineState(state.horizontal, state.depth - x, state.aim)
      }
    }

    def strategyTwo(state:SubmarineState, command: SubmarineCommand): SubmarineState = {
      command match {
        case SubmarineCommand(Forward, x) => SubmarineState(state.horizontal + x, state.depth + (state.aim * x), state.aim)
        case SubmarineCommand(Down, x) => SubmarineState(state.horizontal, state.depth, state.aim + x)
        case SubmarineCommand(Up, x) => SubmarineState(state.horizontal, state.depth, state.aim - x)
      }
    }

    val originalState = commands.map(SubmarineCommand.apply).foldLeft(SubmarineState(0, 0, 0))(strategyOne)
    println(originalState)
    println(originalState.product)

    val updatedState = commands.map(SubmarineCommand.apply).foldLeft(SubmarineState(0, 0, 0))(strategyTwo)
    println(updatedState)
    println(updatedState.product)
  }

  case class SubmarineState(horizontal:BigInt, depth:BigInt, aim: BigInt) {

    def product: BigInt = horizontal * depth

    override def toString: String = s"SubmarineState(horizontal=$horizontal,depth=$depth,aim=$aim)"
  }

  case class SubmarineCommand(direction: Direction, units: Int)

  object SubmarineCommand {
    def apply(command: String): SubmarineCommand = {
      val split = command.split(" ").toList
      SubmarineCommand(Direction(split.head), split(1).toInt)
    }
  }

  sealed trait Direction

  object Direction  {
    case object Forward extends Direction
    case object Down extends Direction
    case object Up extends Direction

    def apply(name: String): Direction =
      name match {
        case "forward" => Forward
        case "down" => Down
        case "up" => Up
        case _ => throw new IllegalArgumentException(s"$name is not a valid command")
      }
  }
}

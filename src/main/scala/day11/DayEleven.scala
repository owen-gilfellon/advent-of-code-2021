package day11

import scala.annotation.tailrec
import scala.io.Source

object DayEleven {

  def main(args: Array[String]): Unit = {
    val filename = "src/main/resources/day11/input.txt"
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val inputOctopuses = inputLines.flatMap(_.map(_.toString.toInt)).map(x => Octopus(x, hasFlashed = false))
    val initialState: Grid[Octopus] = Grid(inputOctopuses, 10)

    val flashesAfter100Iterations = applyAndCount[Octopus](
      grid = initialState,
      neighbourStrategy = octopusFlashes(NeigbourhoodIndex(10, 10).eightNeighbourStrategy),
      countFilter = grid => grid.states.count(_.energy == 0), count = 0,
      iterations = 100)._2

    println(s"Flashes after 100 iterations = $flashesAfter100Iterations")

    val generationsUntilAllFlash = applyAndFind[Octopus](
      grid = initialState,
      neighbourStrategy = octopusFlashes(NeigbourhoodIndex(10, 10).eightNeighbourStrategy),
      findFilter = grid => grid.states.forall(_.energy == 0),
      iteration = 0)

    println(s"Iterations until all flash at once = $generationsUntilAllFlash")
  }

  def octopusFlashes(neighbourhoodFunc: Int => Seq[Int])(grid:Grid[Octopus]): Grid[Octopus] = {
    val increaseEachbyOne = Grid[Octopus](grid.states.map(x => Octopus(x.energy + 1, x.hasFlashed)), grid.width)
    processFlashes(neighbourhoodFunc)(increaseEachbyOne)
  }

  def processFlashes(indexStrategy: Int => Seq[Int])(grid: Grid[Octopus]): Grid[Octopus] = {
    val flashesInNeighbourhood: Grid[Int] = Grid((0 to grid.states.length).map(z => flashesAtIndex(indexStrategy)(grid, z)), grid.width)
    if (!flashesInNeighbourhood.states.exists(x => x > 0))
      Grid(grid.states.map(octopus => if(octopus.hasFlashed) octopus.reset() else octopus), grid.width)
    else {
      val updateFlashes = grid.states.map(octopus => if(octopus.energy > 9 && !octopus.hasFlashed) Octopus(octopus.energy, hasFlashed = true) else octopus)
      processFlashes(indexStrategy)(Grid[Octopus](updateFlashes.zip(flashesInNeighbourhood.states).map((x) => Octopus(x._1.energy + x._2, x._1.hasFlashed)), grid.width))
    }
  }

  def flashesAtIndex(indexStrategy: Int => Seq[Int])(grid:Grid[Octopus], index:Int): Int =
    grid.getNeighbourhood(indexStrategy)(index).count(y => y._1.energy > 9  && !y._1.hasFlashed)

  @tailrec
  def applyAndFind[A](grid: Grid[A], neighbourStrategy: Grid[A] => Grid[A], findFilter: Grid[A] => Boolean, iteration: Int): Int = {
    if(findFilter(grid))
      iteration
    else {
      val updated = neighbourStrategy(grid)
      applyAndFind(updated, neighbourStrategy, findFilter, iteration + 1)
    }
  }

  @tailrec
  def applyAndCount[A](grid: Grid[A], neighbourStrategy: Grid[A] => Grid[A], countFilter: Grid[A] => Int, count: Int, iterations: Int): (Grid[A], Int) = {
    if(iterations == 0)
      (grid, count)
    else {
      val updated = neighbourStrategy(grid)
      applyAndCount(updated, neighbourStrategy, countFilter, count + countFilter(updated), iterations - 1)
    }
  }

  @tailrec
  def applyStrategy[A](grid: Grid[A], f: Grid[A] => Grid[A], iterations: Int): Grid[A] = {
    if(iterations == 0)
      grid
    else
      applyStrategy(f(grid), f, iterations - 1)
  }

  case class NeigbourhoodIndex(gridSizeX:Int, gridSizeY:Int) {

    def fourNeighbourStrategy(x:Int): Seq[Int] =
      Seq(x - gridSizeX, x + gridSizeX) ++ Seq(x - 1, x, x + 1).filter(y => areOnSameLine(x, y))
        .filter(y => y >= 0 && y < gridSizeX * gridSizeY)

    def eightNeighbourStrategy(x:Int): Seq[Int] =
      (Seq(x - gridSizeX - 1, x - gridSizeX, x - gridSizeX + 1).filter(x2 => areOnSameLine(x - gridSizeX, x2)) ++
        Seq(x             - 1,              x             + 1).filter(x2 => areOnSameLine(x, x2)) ++
          Seq(x + gridSizeX - 1, x + gridSizeX, x + gridSizeX + 1).filter(x2 => areOnSameLine(x + gridSizeX, x2)))
          .filter(x2 => x2 >= 0 && x2 < gridSizeX * gridSizeY)

    def areOnSameLine(x:Int, y:Int): Boolean =
      if (x < 0 || y < 0 )
        false
      else
        x / gridSizeX == y / gridSizeX
  }

  case class Octopus(energy: Int, hasFlashed: Boolean) {

    def reset(): Octopus = Octopus(0, hasFlashed = false)

    def +(energy: Int): Octopus = Octopus(this.energy + energy, hasFlashed = this.hasFlashed)

    override def toString: String = {
      if(hasFlashed)
        "*" else energy.toString
    }
  }

  case class Grid[A](states: Seq[A], width: Int) {

    override def toString: String = {
      states.grouped(width).grouped(states.length / width).map(_.map(_.mkString)).map(_.mkString("\n")).mkString
    }

    def getNeighbourhood(indexStrategy: Int => Seq[Int])(x: Int): Seq[(A, Int)] = {
      indexStrategy(x).map(index => (states(index), index))
    }
  }

  object Grid {

    def apply(states: String, gridXSize: Int): Grid[Int] = Grid[Int](states.split("\\n").flatMap(_.map(_.toString.toInt)), gridXSize)
  }

  object OctoGrid {

    def apply(states: String, gridXSize: Int): Grid[Octopus] =
      Grid(states.split("\\n").flatMap(_.map(_.toString.toInt)
        .map(x => Octopus(x, hasFlashed = false))), gridXSize)
  }
}

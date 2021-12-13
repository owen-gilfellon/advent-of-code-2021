package day12

import scala.io.Source

object DayTwelve {

  def main(args: Array[String]): Unit = {
    val filename = "src/main/resources/day12/input.txt"
    val reader = Source.fromFile(filename)
    val inputLines: Seq[String] = reader.getLines.toSeq
    reader.close()

    val corridors = inputLines.map(x => {
      val splitString = x.split("-")
      Corridor(Cave(splitString(0)), Cave(splitString(1)))
    }).toSet

    val caveSystem: CaveSystem = CaveSystem(corridors.flatMap(c => Seq(c.from, c.to)), corridors)

    println(s"Number of paths visiting small caves at most once:  ${enumeratePaths(allSmallCavesAtMostOnce)(caveSystem).size}")
    println(s"Number of paths visiting one small cave at most twice:  ${enumeratePaths(oneSmallCaveTwice)(caveSystem).size}")

  }

  def allSmallCavesAtMostOnce(path: Path): Boolean = {
    val smallCaves =  path.caves.filter(c => !c.isLarge)
    smallCaves.size == smallCaves.distinct.size
  }

  def oneSmallCaveTwice(path: Path): Boolean = {
    val smallCaves =  path.caves.filter(c => !c.isLarge)
    val startOnce = smallCaves.count(c => c.label == "start") == 1
    val endAtMostOnce = smallCaves.count(c => c.label == "end") <= 1
    val onlyVisitedOnce = smallCaves.size == smallCaves.distinct.size
    val oneVisitedTwice =  smallCaves.size == smallCaves.distinct.size + 1
    (onlyVisitedOnce || oneVisitedTwice) && startOnce && endAtMostOnce
  }

  def enumeratePaths(pathFilter: Path => Boolean)(caveSystem: CaveSystem): Set[Path] =
    enumeratePaths(pathFilter, caveSystem, Set(Path(Seq(caveSystem.getCave("start")))), Set.empty)

  def enumeratePaths(pathFilter: Path => Boolean, caveSystem: CaveSystem, currentPaths: Set[Path], completePaths: Set[Path]): Set[Path]  = {

    val potentialPaths: Set[Path] = currentPaths
      .flatMap(path => caveSystem.getCavesAccessibleFrom(path.caves.last).map(cave => path + cave))
      .filter(pathFilter)

    if(potentialPaths.forall(p => p.isComplete)) {
      potentialPaths ++ completePaths
    } else {
      enumeratePaths(pathFilter, caveSystem, potentialPaths.filterNot(_.isComplete), completePaths ++ potentialPaths.filter(_.isComplete))
    }
  }

  case class CaveSystem(caves: Set[Cave], corridors: Set[Corridor]) {

    private val caveMap = caves.map(c => (c.label, c)).toMap

    def getCavesAccessibleFrom(c: Cave): Set[Cave] = getCorridors(c).map(x => if (x.from != c) x.from else x.to)

    def getCorridors(cave: Cave): Set[Corridor] = corridors.filter(c => c.from == cave || c.to == cave)

    def getCave(label: String): Cave = caveMap(label)
  }

  case class Path(caves: Seq[Cave]) {

    def +(cave: Cave): Path = Path(caves :+ cave)

    def isComplete: Boolean = caves.last.label == "end"

    override def toString: String = caves.mkString(",")
  }

  case class Cave(label: String) {

    def isLarge: Boolean =  label.matches("[A-Z]+")

    override def toString: String = label
  }

  case class Corridor(from: Cave, to: Cave) {

    override def toString: String = s"$from-$to"
  }
}

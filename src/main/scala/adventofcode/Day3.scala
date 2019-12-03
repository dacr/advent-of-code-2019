package adventofcode

import better.files._

import scala.math._

object Day3 {

  case class Point(x: Int, y: Int) {
    def addToX(deltaX: Int) = Point(x + deltaX, y)

    def addToY(deltaY: Int) = Point(x, y + deltaY)
  }

  def manhattanDistance(a: Point, b: Point): Int = abs(a.x - b.x) + abs(a.y - b.y)

  object Part1 {

    def fromMovesToCoordinates(moves: List[String], position: Point = Point(0, 0), points: List[Point] = Nil): List[Point] = {
      moves match {
        case Nil => points
        case move :: tail =>
          val direction = move.head
          val movecount = move.tail.toInt
          val newPoints = direction match {
            case 'U' => (1 to movecount).map(i => position.addToY(i))
            case 'D' => (1 to movecount).map(i => position.addToY(-i))
            case 'L' => (1 to movecount).map(i => position.addToX(-i))
            case 'R' => (1 to movecount).map(i => position.addToX(i))
          }
          val newPosition = newPoints.last
          fromMovesToCoordinates(tail, newPosition, points ++ newPoints)
      }
    }

    def closestIntersection(pathStrings: String): Int = {
      val movesPaths = pathStrings.split("\n").map(_.trim).map(_.split(",").toList)
      val a = fromMovesToCoordinates(movesPaths(0))
      val b = fromMovesToCoordinates(movesPaths(1))
      val intersections = a.intersect(b)
      val distances = intersections.map(p => manhattanDistance(Point(0, 0), p))
      distances.min
    }

    def computeForInputFile(): Int = {
      val inputFile = "data" / "day3" / "part1" / "input.txt"
      closestIntersection(inputFile.contentAsString)
    }
  }

  object Part2 {

    def fromMovesToCoordinates(moves: List[String], position: Point = Point(0, 0), distance:Int = 0, points: List[Point] = Nil, distances:Map[Point,Int]=Map.empty): (List[Point], Map[Point,Int]) = {
      moves match {
        case Nil => (points, distances)
        case move :: tail =>
          val direction = move.head
          val movecount = move.tail.toInt
          val newPoints  = direction match {
            case 'U' => (1 to movecount).map(i => position.addToY(i))
            case 'D' => (1 to movecount).map(i => position.addToY(-i))
            case 'L' => (1 to movecount).map(i => position.addToX(-i))
            case 'R' => (1 to movecount).map(i => position.addToX(i))
          }
          val newDistances = newPoints.map(newPoint => newPoint -> (distance + manhattanDistance(position,newPoint)))
          val newPosition = newPoints.last
          val newDistance = distance + movecount
          fromMovesToCoordinates(tail, newPosition, newDistance, points ++ newPoints, distances ++ newDistances)
      }
    }


    def bestIntersection(pathStrings: String): Int = {
      val movesPaths = pathStrings.split("\n").map(_.trim).map(_.split(",").toList)
      val (pointsA, distancesA) = fromMovesToCoordinates(movesPaths(0))
      val (pointsB, distancesB) = fromMovesToCoordinates(movesPaths(1))
      val intersections = pointsA.intersect(pointsB)
      val distanceSumsForIntersections = intersections.map(point => distancesA(point) + distancesB(point))
      distanceSumsForIntersections.min
    }

    def computeForInputFile(): Int = {
      val inputFile = "data" / "day3" / "part1" / "input.txt"
      bestIntersection(inputFile.contentAsString)
    }
  }

}

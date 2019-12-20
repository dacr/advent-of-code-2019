package adventofcode

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import better.files._

import scala.annotation.tailrec

object Day18 {
  def fileToString(inputFile: File = "data" / "day18" / "input.txt"): String = inputFile.contentAsString

  case class Solution(shortestPathLength:Int, collectedKeys:Vector[Item]) {
    override def toString: String = shortestPathLength+" "+collectedKeys.map(_.name).mkString
  }

  type Position = (Int,Int)

  sealed trait Direction {
    val reverse: Direction
    val turnLeft: Direction
    val turnRight: Direction
  }

  object Up extends Direction {
    val reverse = Down
    val turnLeft = Left
    val turnRight = Right
  }

  object Down extends Direction {
    val reverse = Up
    val turnLeft = Right
    val turnRight = Left
  }

  object Left extends Direction {
    val reverse = Right
    val turnLeft = Down
    val turnRight = Up
  }

  object Right extends Direction {
    val reverse = Left
    val turnLeft = Up
    val turnRight = Down
  }

  val directions = List(Up,Down,Left,Right)


  def move(position: Position)(direction: Direction): Position = position match {
    case (x, y) =>
      direction match {
        case Up => (x, y - 1)
        case Left => (x - 1, y)
        case Down => (x, y + 1)
        case Right => (x + 1, y)
      }
  }


  trait Tile
  object Wall extends Tile {
    override def toString: String = "#"
  }
  object Free extends Tile {
    override def toString: String = "."
  }
  object Start extends Tile {
    override def toString: String = "@"
  }

  case class Item(name:Char) extends Tile {
    val isKey = name.isLower
    val isDoor = name.isUpper
    override def toString: String = name.toString
  }


  case class Land(zones: Vector[Vector[Tile]]) {
    val width = zones.head.size
    val height = zones.size

    override def toString() = zones.map(_.mkString).mkString("\n")

    def get(pos: Position): Tile = pos match {
      case (x, y) => zones(y)(x)
    }

    def safeGet(pos:Position):Option[Tile] = pos match {
      case (x,y) if x < 0 || y < 0 || x > width-1 || y > height-1 => None
      case (x,y) => Some(zones(y)(x))
    }

    def set(pos: Position, tile: Tile): Land = pos match {
      case (x, y) => Land(zones.updated(y, zones(y).updated(x, tile)))
    }

    def search(criteria: Tile=>Boolean):IndexedSeq[(Tile,Position)] = {
      for {
        x <- 0 until width
        y <- 0 until height
        pos = (x, y)
        tile = get(pos)
        if criteria(tile)
      } yield tile->pos
    }
    def startPosition():Option[Position] = {
      search(tile => tile == Start).headOption.collect {
        case (_,position) => position
      }
    }
    def possibleMoves(from:Position): List[Position] = {
      directions.map(direction => move(from)(direction)).filter(position => get(position) != Wall)
    }

    def clean(pos: Position):Land = set(pos,Free)
  }

  object Land {
    def charToTile(value: Char):Tile = value match {
      case '.' => Free
      case '#' => Wall
      case '@' => Start
      case x => Item(x)
    }

    def apply(width: Int, height: Int): Land = {
      Land(Vector.fill(height)(Vector.fill(width)(Free)))
    }
    def apply(landString: String): Land = {
      Land(
        landString
          .split("\n")
          .toVector
          .map(row => row.toCharArray.toVector.map(charToTile))
      )
    }
  }

  case class ItemPos(item:Item, position: Position)

  case class Choice(itemPos:ItemPos, distance:Int) {
    def isKey = itemPos.item.isKey
    def isDoor = itemPos.item.isDoor
  }

  def searchChoices(from:Position, lab:Land):List[Choice] = {
    @tailrec
    def bfsWorker(notVisited:Set[Position],visited:Set[Position], depth:Int=1, choices:List[Choice]=List.empty):List[Choice] = {
      if (notVisited.isEmpty) choices
      else {
        val newPositions =
          notVisited
            .flatMap(lab.possibleMoves)
            .filterNot(visited)
        val (newChoicePosition,newNotVisited) = newPositions.partition(position => lab.get(position).isInstanceOf[Item]) // TODO BAD
        val newChoices = newChoicePosition.collect(pos => Choice(ItemPos(lab.get(pos).asInstanceOf[Item],pos), distance = depth)) // TODO BAD
        bfsWorker(newNotVisited, visited++notVisited, depth+1, choices ++ newChoices)
      }
    }
    bfsWorker(Set(from),Set.empty)
  }


  def canBeOpenedBy(doorChoice: Choice, key: Item): Boolean = {
    doorChoice.itemPos.item.name == key.name.toUpper
  }

  def solve(labyrinthString:String):List[Solution] = {
    val lab = Land(labyrinthString)
    val initialPos = lab.startPosition().get // TODO BAD
    val cleanedLab = lab.clean(initialPos)

    var currentBestDistance = Int.MaxValue // TODO BAD !
    var currentBestSolutions = List.empty[Solution]

    var currentBestMidDistance = Int.MaxValue
    val currentSolutionLength = lab.toString.count(ch => ch.isLetter && ch.isLower)

    var squeezed = 0

    @inline def stopHeuristicCond1(distance:Int, collectKeys:Vector[Item]):Boolean = {
      if (distance > currentBestDistance) true else {
        if (collectKeys.size == currentSolutionLength/3) {
          if (distance < currentBestMidDistance) {
            currentBestMidDistance = distance
          }
          distance > 2.5*currentBestMidDistance/2
        } else false
      }
    }
    @inline def stopHeuristicCond2(distance:Int, collectKeys:Vector[Item]):Boolean = {
      //distance >= currentBestDistance // to get first best solution with shortest path
      distance > currentBestDistance // to all best solution with the same shortest path
    }

    def explore(pos:Position, lab:Land, distance:Int, availableKeys:Set[Char], collectedKeys:Vector[Item]):Unit = {
      if ( stopHeuristicCond2(distance,collectedKeys) ) {
        squeezed+=1
        Nil
      } else {
        val choices = searchChoices(pos, lab)
        if (choices.isEmpty) {
          val solution = Solution(distance, collectedKeys)
          if (distance < currentBestDistance) currentBestSolutions = Nil
          currentBestDistance = distance
          currentBestSolutions ::= solution
          println(s"currentBest : $solution #${currentBestSolutions.size} $currentBestMidDistance ($squeezed)")
        } else {
          // Either collect new keys or open doors
          //scala.util.Random.shuffle(choices).foreach {
          choices.foreach {
            case collectableKey if collectableKey.isKey => // isKey
              val newPosition = collectableKey.itemPos.position
              val newLab = lab.clean(collectableKey.itemPos.position)
              val newDistance = distance + collectableKey.distance
              val newCollectedKeys = collectedKeys.appended(collectableKey.itemPos.item)
              val newAvailableKeys = availableKeys + collectableKey.itemPos.item.name
              explore(pos = newPosition, lab = newLab, distance = newDistance, availableKeys = newAvailableKeys, collectedKeys = newCollectedKeys)

            case openableDoor if availableKeys.contains(openableDoor.itemPos.item.name.toLower) =>
              val newPosition = openableDoor.itemPos.position
              val newLab = lab.clean(openableDoor.itemPos.position)
              val newDistance = distance + openableDoor.distance
              val newCollectedKeys = collectedKeys
              val newAvailableKeys = availableKeys - openableDoor.itemPos.item.name.toLower
              explore(pos = newPosition, lab = newLab, distance = newDistance, availableKeys = newAvailableKeys, collectedKeys = newCollectedKeys)

            case _ =>
          }

        }
      }
    }

    explore(initialPos, cleanedLab,0, Set.empty, Vector.empty)
    currentBestSolutions
  }

  def main(args: Array[String]): Unit = {
    val solution =
      solve(fileToString())
        .groupBy(_.shortestPathLength)
        .minByOption{case(len,sols) => len}
        .map{case (len,sols) => len->sols.size}

    println(solution)
  }
}

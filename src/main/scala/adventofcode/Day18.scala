package adventofcode

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import better.files._

object Day18 {
  def fileToString(inputFile: File = "data" / "day18" / "input.txt"): String = inputFile.contentAsString

  case class Solution(shortestPathLength:Int, shortestPaths:List[List[Char]])

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

  trait Item extends Tile

  case class Door(name:Char) extends Item {
    override def toString: String = name.toString
  }

  case class Key(name:Char) extends Item {
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
    def startPosition():Option[(Tile,Position)] = {
      search(tile => tile == Start).headOption
    }
  }

  object Land {
    def charToTile(value: Char):Tile = value match {
      case '.' => Free
      case '#' => Wall
      case '@' => Start
      case x if x.isLower => Key(x)
      case x if x.isUpper => Door(x)
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

  case class Choice(item:Item, distance:Int)

  def choices(from:Position, lab:Land):List[Choice] = {
    ???
  }


  def solve(labyrinthString:String):Solution = {
    val lab = Land(labyrinthString)
    val pos = lab.startPosition()
    ???
  }
}

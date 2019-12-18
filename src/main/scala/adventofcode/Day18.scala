package adventofcode

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import better.files._

object Day18 {
  def fileToString(inputFile: File = "data" / "day18" / "input.txt"): String = inputFile.contentAsString

  case class Solution(shortestPathLength:Int, shortestPaths:List[List[Char]])

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
  case class Door(name:Char) extends Tile {
    override def toString: String = name.toString
  }
  case class Key(name:Char) extends Tile {
    override def toString: String = name.toString
  }


  def solve(labyrinthString:String):Solution = {
    ???
  }
}

package adventofcode

import better.files._


object Day10 {

  trait Zone
  object Free extends Zone
  object Asteroid extends Zone

  case class Area(zones:Array[Array[Zone]]) {
    val width = zones.head.size
    val height = zones.size
    def get(x:Int,y:Int):Zone = zones(y)(x)
    override def toString() = zones.map(_.map{case Free => "." case Asteroid => "#"}.mkString).mkString("\n")
  }

  def countVisible(area: Area, zoneX: Int, zoneY: Int): Int = {
    val posX=zoneX+0.5d
    val posY=zoneY+0.5d
    var mutatedArea = area.copy()
    for {
      altZoneY <- 0 until area.height
      altZoneX <- 0 until area.width
      if altZoneX != zoneX && altZoneY != zoneY
      altY=altZoneY+0.5d
      altX=altZoneX+0.5d
    } yield {

    }
  }

  def searchBestAsteroid(area:Area):(Int,Int) = {
    val result = for {
      y <- 0 until area.height
      x <- 0 until area.width
      if area.get(x,y)==Asteroid
    } yield {
      (x,y) -> countVisible(area,x,y)
    }

    result.maxBy{case (_, count) => count} match {case (pos,_)=>pos}
  }

  def stringToArea(areaString: String): Area = {
    Area(
      areaString
        .split("\n")
        .map(row => row.toCharArray.map{case '.' => Free case '#' => Asteroid})
    )
  }

  def fileToArea(inputFile: File = "data" / "day10" / "part1" / "input.txt"): Area = {
    stringToArea(inputFile.contentAsString)
  }

}

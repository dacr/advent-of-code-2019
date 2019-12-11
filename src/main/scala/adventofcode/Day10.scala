package adventofcode

import better.files._


object Day10 {

  trait Zone
  object Free extends Zone
  object Asteroid extends Zone

  case class Area(zones:Array[Array[Zone]]) {
    val width = zones.head.size
    val height = zones.size
    def get(x:Int,y:Int):Zone = {
      if (y < 0 || x < 0 || y>= height || x>= width) Free
      else zones(y)(x)
    }
    override def toString() = zones.map(_.map{case Free => "." case Asteroid => "#"}.mkString).mkString("\n")
  }

  def isHidden(area: Area, altPosX: BigDecimal, altPosY: BigDecimal, posX: BigDecimal, posY: BigDecimal): Boolean = {
    var dx = (altPosX - posX)
    var dy = (altPosY - posY)
    val step = if (dx.abs >= dy.abs ) dx.abs else dy.abs
    dx = dx / step
    dy = dy / step
    var x = posX
    var y = posY
    var i=1
    var result = false
    while(i < step  && !result) {
      x += dx
      y += dy
      i += 1
      if ( area.get(x.toInt, y.toInt) == Asteroid &&
        ( (x - x.toInt - 0.5d).abs < 0.001d) && ( (y - y.toInt - 0.5d).abs < 0.001d ) // TAKE CARE OF ERROR MARGIN, when step=0.333333... for example
      ) result = true
    }
    result
  }

  def countVisible(area: Area, zoneX: Int, zoneY: Int): Int = {
    val posX=BigDecimal(zoneX)+0.5d
    val posY=BigDecimal(zoneY)+0.5d
    val result = for {
      altZoneY <- 0 until area.height
      altZoneX <- 0 until area.width
      if altZoneX != zoneX || altZoneY != zoneY
      if area.get(altZoneX, altZoneY) == Asteroid
      altPosX=BigDecimal(altZoneX)+0.5d
      altPosY=BigDecimal(altZoneY)+0.5d
    } yield {
      if (isHidden(area, altPosX, altPosY, posX, posY)) 0 else 1
    }
    result.sum
  }

  def resultString(area:Area):String = {
      0.until(area.height).map { y =>
        0.until(area.width).map { x =>
          if (area.get(x, y) == Asteroid) countVisible(area, x, y).toString
          else "."
        }.mkString
      }.mkString("\n")
  }

  def searchBestAsteroid(area:Area):Int = {
    val result = for {
      y <- 0 until area.height
      x <- 0 until area.width
      if area.get(x,y)==Asteroid
    } yield {
      (x,y) -> countVisible(area,x,y)
    }

    result
      .maxByOption{case (_, count) => count}
      .map {case (pos,count)=>count}
      .getOrElse(0)
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

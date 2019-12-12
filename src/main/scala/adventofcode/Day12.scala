package adventofcode

import better.files._
import scala.math._

object Day12 {
  object Part1 {

    case class Vect(x:Double, y:Double, z:Double) {
      def add(that:Vect):Vect = Vect(x+that.x, y+that.y, z+that.z)
    }

    case class Moon(id:Int, position:Vect, velocity: Vect)

    def gravityImpactFor(that: Moon, becauseOfThis: Moon): Vect = {
      def impact(v1:Double,v2:Double):Double = if (v1 > v2 ) +1d else if (v1 < v2) -1d else 0d
      Vect(
        x=impact(that.velocity.x, becauseOfThis.velocity.x),
        y=impact(that.velocity.y, becauseOfThis.velocity.y),
        z=impact(that.velocity.z, becauseOfThis.velocity.z),
      )
    }

    def simulate(moons:List[Moon]): Unit = {
      // update velocity by applying gravity
      val velocityChanges =
        moons
          .combinations(2)
          .toList
          .flatMap{case List(a,b) => List(a->gravityImpactFor(a,b), b->gravityImpactFor(b,a)) }
          .groupMapReduce{case (m, _)=> m}{case (_,c) => c}{ (c1, c2) => c1.add(c2)}

      // update position by applying velocity

      // increase time step by one
    }

    def parse(input:String):List[Moon] = {
      input
        .replaceAll("[<>xyz=]", "")
        .split("\n")
        .map(_.split("""\s*,\s*"""))
        .zipWithIndex
        .collect{case (Array(xs,ys,zs), id)=> Moon(id, Vect(xs.toDouble, ys.toDouble, zs.toDouble), Vect(0,0,0))}
        .toList
    }
    def fileToString(inputFile: File = "data" / "day12" / "input.txt"): String = inputFile.contentAsString


  }

  object Part2 {
  }
}

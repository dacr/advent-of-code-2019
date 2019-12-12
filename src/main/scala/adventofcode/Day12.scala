package adventofcode

import better.files._
import scala.math._

object Day12 {
  object Part1 {

    case class Vect(x:Long, y:Long, z:Long) {
      def add(that:Vect):Vect = Vect(x+that.x, y+that.y, z+that.z)
      def absolutesSum=abs(x)+abs(y)+abs(z)
    }

    case class Moon(id:Int, position:Vect, velocity: Vect) {
      def move(newVelocity:Vect) = Moon(id, position.add(newVelocity), newVelocity)
      def pot = position.absolutesSum
      def kin = velocity.absolutesSum
      def totalEnergy = pot*kin
    }

    def gravityImpactFor(that: Moon, becauseOfThis: Moon): Vect = {
      def impact(v1:Long,v2:Long):Long = if (v1 < v2 ) +1 else if (v1 > v2) -1 else 0
      Vect(
        x=impact(that.position.x, becauseOfThis.position.x),
        y=impact(that.position.y, becauseOfThis.position.y),
        z=impact(that.position.z, becauseOfThis.position.z),
      )
    }

    def simulate(moons:List[Moon]): Long = {
      var currentMoons:List[Moon] = moons
      (1 to 1000) map {  step =>
        // update velocity by applying gravity
        val velocityChanges =
          currentMoons
            .combinations(2)
            .toList
            .flatMap { case List(a, b) => List(a -> gravityImpactFor(a, b), b -> gravityImpactFor(b, a)) }
            .groupMapReduce { case (m, _) => m } { case (_, c) => c } { (c1, c2) => c1.add(c2) }

        // update position by applying velocity
        currentMoons = velocityChanges.map { case (moon, changes) => moon.move(moon.velocity.add(changes)) }.toList
      }
      currentMoons.map(_.totalEnergy).sum
    }

    def parse(input:String):List[Moon] = {
      input
        .replaceAll("[<>xyz=]", "")
        .split("\n")
        .map(_.split("""\s*,\s*"""))
        .zipWithIndex
        .collect{case (Array(xs,ys,zs), id)=> Moon(id, Vect(xs.toLong, ys.toLong, zs.toLong), Vect(0,0,0))}
        .toList
    }
    def fileToString(inputFile: File = "data" / "day12" / "input.txt"): String = inputFile.contentAsString


  }

  object Part2 {
  }
}

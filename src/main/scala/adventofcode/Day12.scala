package adventofcode

import better.files._
import scala.math._

object Day12 {
  object Part1 {

    class Vect(val x:Int, val y:Int, val z:Int) {
      def add(that:Vect):Vect = new Vect(x+that.x, y+that.y, z+that.z)
      def absolutesSum: Int =abs(x)+abs(y)+abs(z)
    }

    case class Moon(id:Int, position:Vect, velocity: Vect) {
      def move(newVelocity:Vect) = Moon(id, position.add(newVelocity), newVelocity)
      def pot = position.absolutesSum
      def kin = velocity.absolutesSum
      def totalEnergy = pot*kin

      val hash = 31*(id + 31L*(position.x + 31L*(position.y + 31L*(position.z + 31L*(velocity.x + 31L*(velocity.y + 31L*velocity.z))))))
    }

    def gravityImpactFor(that: Moon, becauseOfThis: Moon): Vect = {
      def impact(v1:Int,v2:Int):Int = if (v1 < v2 ) +1 else if (v1 > v2) -1 else 0
      new Vect(
        x=impact(that.position.x, becauseOfThis.position.x),
        y=impact(that.position.y, becauseOfThis.position.y),
        z=impact(that.position.z, becauseOfThis.position.z),
      )
    }

    def simulate(moons:List[Moon]): Int = {
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

    def moonsHash(moons:List[Moon]):Long = moons.foldLeft(0L) { case (code, c) => 31*code + c.hash }


    def howManyStepsToGoBackToAnAlreadySeenState(moons:List[Moon]): Long = {
      //var knownStates = Set.empty[Long]

      val referenceHash = moonsHash(moons)
      var currentMoons:List[Moon] = moons
      var currentHash:Long = -1
      var steps = 0L
      println("START")
      while(currentHash != referenceHash)  {
        if (steps % 1000000L == 0L) println(steps)
        val velocityChanges =
          currentMoons
            .combinations(2)
            .toList
            .flatMap { case List(a, b) => List(a -> gravityImpactFor(a, b), b -> gravityImpactFor(b, a)) }
            .groupMapReduce { case (m, _) => m } { case (_, c) => c } { (c1, c2) => c1.add(c2) }

        currentMoons = velocityChanges.map { case (moon, changes) => moon.move(moon.velocity.add(changes)) }.toList
        currentHash = moonsHash(currentMoons)
        steps+=1L
      }
      steps
    }

    def parse(input:String):List[Moon] = {
      val primes = List(3,5,7,11,13,17)
      input
        .replaceAll("[<>xyz=]", "")
        .split("\n")
        .map(_.split("""\s*,\s*"""))
        .zip(primes)
        .collect{case (Array(xs,ys,zs), id)=> Moon(id, new Vect(xs.toInt, ys.toInt, zs.toInt), new Vect(0,0,0))}
        .toList
    }
    def fileToString(inputFile: File = "data" / "day12" / "input.txt"): String = inputFile.contentAsString

  }


}

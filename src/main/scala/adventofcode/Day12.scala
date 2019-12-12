package adventofcode

import better.files._
import scala.math._

object Day12 {
  object Part1 {

    case class Vect(x:Long, y:Long, z:Long) {
      def add(that:Vect):Vect = Vect(x+that.x, y+that.y, z+that.z)
      def absolutesSum: Long =abs(x)+abs(y)+abs(z)
      //val hash:Long = scala.util.hashing.MurmurHash3.stringHash(toString)
    }

    case class Moon(id:Int, position:Vect, velocity: Vect) {
      def move(newVelocity:Vect) = Moon(id, position.add(newVelocity), newVelocity)
      def pot = position.absolutesSum
      def kin = velocity.absolutesSum
      def totalEnergy = pot*kin
      //val hash:Long = scala.util.hashing.MurmurHash3.stringHash(toString)
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

    //val primes = List(127, 131, 137, 139, 149, 151, 157, 163, 167, 173)
    case class MoonList(moons:List[Moon]) {
      //val hash = moons.zip(primes).map{ case (moon,prime) => moon.id*moon.hash*prime}.sum
      //val hash:Int = scala.util.hashing.MurmurHash3.stringHash(toString)
      //val hash:Int = scala.util.hashing.MurmurHash3.listHash(moons, 42)
      val hash:Long = moons.toString().foldLeft(0L) { case (code, c) => 31*code + c }
    }

    def howManyStepsToGoBackToAnAlreadySeenState(moons:List[Moon]): Long = {
      var knownStates = Set.empty[Long]

      var currentMoons:MoonList = MoonList(moons)
      var steps = 0L
      println("START")
      while(! knownStates.contains(currentMoons.hash))  {
        knownStates += currentMoons.hash
        if (steps % 100000L == 0L) println(steps)
        // update velocity by applying gravity
        val velocityChanges =
          currentMoons.moons
            .combinations(2)
            .toList
            .flatMap { case List(a, b) => List(a -> gravityImpactFor(a, b), b -> gravityImpactFor(b, a)) }
            .groupMapReduce { case (m, _) => m } { case (_, c) => c } { (c1, c2) => c1.add(c2) }

        // update position by applying velocity
        currentMoons = MoonList(velocityChanges.map { case (moon, changes) => moon.move(moon.velocity.add(changes)) }.toList)
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
        .collect{case (Array(xs,ys,zs), id)=> Moon(id, Vect(xs.toLong, ys.toLong, zs.toLong), Vect(0,0,0))}
        .toList
    }
    def fileToString(inputFile: File = "data" / "day12" / "input.txt"): String = inputFile.contentAsString


  }


}

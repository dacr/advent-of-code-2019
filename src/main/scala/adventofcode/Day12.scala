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
    type NumType = Int
    type HashType = Long

    class Vect(var x:NumType, var y:NumType, var z:NumType) {
      def absolutesSum: NumType =abs(x)+abs(y)+abs(z)
    }

    class Moon(id:NumType, var x:NumType,var y:NumType, var z:NumType, var vx:NumType, var vy:NumType, var vz:NumType) {
      def move():Unit = {
        x = x + vx
        y = y + vy
        z = z + vz
      }
      def pot = abs(x)+abs(y)+abs(z)
      def kin = abs(vx)+abs(vy)+abs(vz)
      def totalEnergy:NumType = pot*kin

      def hash:NumType = 31*(id + 31*(x + 31*(y + 31*(z + 31*(vx + 31*(vy + 31*vz))))))

      override def toString: String = {
        f"$id : $x%+04d/$y%+04d/$z%+04d $vx%+04d/$vy%+04d/$vz%+04d"
      }
    }

    @inline def moonsHash(moons:List[Moon]):HashType = moons.foldLeft(0L) { case (code, c) => 31*code + c.hash }
    @inline def moonsHash(moons:Array[Moon]):HashType = {
      var code = 0L
      var i = 0
      while(i < moons.length) {
        code = 31L*code + moons(i).hash
        i+=1
      }
     code
    }


    def howManyStepsToGoBackToAnAlreadySeenState(moons:List[Moon], limit:Long=Long.MaxValue): Long = {
      //var knownStates = Set.empty[Long]

      val referenceHash = moonsHash(moons)
      val currentMoons:Array[Moon] = moons.toArray
      var currentHash:Long = -1L
      var steps = 0L
      val indexCombinationsA = 0.until(moons.size).combinations(2).map{case IndexedSeq(a,b) => a}.toArray
      val indexCombinationsB = 0.until(moons.size).combinations(2).map{case IndexedSeq(a,b) => b}.toArray

      println("START")
      var i=0
      //while(currentHash != referenceHash && steps < limit)  {
      while(currentHash != referenceHash)  {
        //if (steps % 100_000_000L == 0L) println(steps)

        //println(currentMoons.mkString(", "))

        i=0
        while(i < indexCombinationsA.length) {
          val a = currentMoons(indexCombinationsA(i))
          val b = currentMoons(indexCombinationsB(i))

          if (a.x < b.x) {
            a.vx += 1
            b.vx -= 1
          } else if (a.x > b.x) {
            a.vx -= 1
            b.vx += 1
          }

          if (a.y < b.y) {
            a.vy += 1
            b.vy -= 1
          } else if (a.y > b.y) {
            a.vy -= 1
            b.vy += 1
          }

          if (a.z < b.z) {
            a.vz += 1
            b.vz -= 1
          } else if (a.z > b.z) {
            a.vz -= 1
            b.vz += 1
          }

          i+=1
        }

        i=0
        while (i <  currentMoons.length) {
          currentMoons(i).move
          i+=1
        }

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
        .collect{case (Array(xs,ys,zs), id)=> new Moon(id, xs.toInt, ys.toInt, zs.toInt, 0,0,0)}
        .toList
    }
    def fileToString(inputFile: File = "data" / "day12" / "input.txt"): String = inputFile.contentAsString

  }


}

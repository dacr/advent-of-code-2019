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

    class Vect(var x:Long, var y:Long, var z:Long) {
      def add(that:Vect):Unit = {
        x += that.x
        y += that.y
        z += that.z
      }
      def set(that:Vect):Unit = {
        x = that.x
        y = that.y
        z = that.z
      }
      def reset():Unit = {
        x=0
        y=0
        z=0
      }
      def absolutesSum: Long =abs(x)+abs(y)+abs(z)
    }

    case class Moon(id:Long, position:Vect, velocity: Vect) {
      def move() = {
        position.add(velocity)
      }
      def pot = position.absolutesSum
      def kin = velocity.absolutesSum
      def totalEnergy = pot*kin

      def hash = 31*(id + 31L*(position.x + 31L*(position.y + 31L*(position.z + 31L*(velocity.x + 31L*(velocity.y + 31L*velocity.z))))))
    }

    @inline def moonsHash(moons:List[Moon]):Long = moons.foldLeft(0L) { case (code, c) => 31L*code + c.hash }
    @inline def moonsHash(moons:Array[Moon]):Long = {
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
      while(currentHash != referenceHash && steps < limit)  {
        if (steps % 100_000_000L == 0L) println(steps)

        i=0
        while(i < indexCombinationsA.length) {
          val a = currentMoons(indexCombinationsA(i))
          val b = currentMoons(indexCombinationsB(i))

          if (a.position.x < b.position.x) {
            a.velocity.x += 1L
            b.velocity.x -= 1L
          } else if (a.position.x > b.position.x) {
            a.velocity.x -= 1L
            b.velocity.x += 1L
          }

          if (a.position.y < b.position.y) {
            a.velocity.y += 1L
            b.velocity.y -= 1L
          } else if (a.position.y > b.position.y) {
            a.velocity.y -= 1L
            b.velocity.y += 1L
          }

          if (a.position.z < b.position.z) {
            a.velocity.z += 1L
            b.velocity.z -= 1L
          } else if (a.position.z > b.position.z) {
            a.velocity.z -= 1L
            b.velocity.z += 1L
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
        .collect{case (Array(xs,ys,zs), id)=> Moon(id, new Vect(xs.toInt, ys.toInt, zs.toInt), new Vect(0,0,0))}
        .toList
    }
    def fileToString(inputFile: File = "data" / "day12" / "input.txt"): String = inputFile.contentAsString

  }


}

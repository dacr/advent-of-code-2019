package adventofcode

import better.files._

import scala.annotation.tailrec
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
    type NumType = Long

    def fileToString(inputFile: File = "data" / "day12" / "input.txt"): String = inputFile.contentAsString

    def parse(input:String):List[Moon] = {
      input
        .replaceAll("[<>xyz=]", "")
        .split("\n")
        .map(_.split("""\s*,\s*"""))
        .collect{case Array(xs,ys,zs)=> Moon(Vect(xs.toLong, ys.toLong, zs.toLong), Vect(0L,0L,0L))}
        .toList
    }

    case class Vect(x:NumType, y:NumType, z:NumType)
    case class Moon(position:Vect, velocity: Vect)


    @tailrec
    def gcd(a: BigInt, b: BigInt):BigInt=if (b == 0) a.abs else gcd(b, a%b)
    def lcm(a: BigInt, b: BigInt):BigInt=(a*b).abs/gcd(a,b)
    def lcms(nums:Iterable[BigInt]):BigInt = nums.reduce((a,b)=>lcm(a,b))
    def lcms(nums:BigInt*):BigInt = nums.reduce((a,b)=>lcm(a,b))

    def adjustGravity(ap: NumType, bp: NumType):NumType = {
      if (ap < bp) +1L else if (ap > bp) -1L else 0L
    }


    def computeCycles(referencePositions: Array[NumType], referenceVelocities:Array[NumType]): NumType = {
      var positions = referencePositions.clone()
      var velocities = referenceVelocities.clone()
      var steps = 0
      var indexCombinations = 0.until(referencePositions.size).combinations(2).toList.map{case IndexedSeq(ia,ib)=> (ia,ib)}
      while(steps == 0 || !referencePositions.sameElements(positions) || !referenceVelocities.sameElements(velocities)) {
        indexCombinations.foreach{case (ia,ib)=>
          val ap = positions(ia)
          val bp = positions(ib)
          velocities(ia) +=  adjustGravity(ap,bp)
          velocities(ib) +=  adjustGravity(bp,ap)
        }
        for{i <- 0.until(positions.length)} {
          positions(i)+=velocities(i)
        }
        steps+=1
      }
      steps
    }

    def howManyStepsToGoBackToAnAlreadySeenState(moons:List[Moon]): BigInt = {
      val xcycles:NumType = computeCycles(moons.map(m => m.position.x).toArray, moons.map(m => m.velocity.x).toArray)
      val ycycles:NumType = computeCycles(moons.map(m => m.position.y).toArray, moons.map(m => m.velocity.y).toArray)
      val zcycles:NumType = computeCycles(moons.map(m => m.position.z).toArray, moons.map(m => m.velocity.z).toArray)
      lcms(xcycles,ycycles,zcycles)
    }

  }


}

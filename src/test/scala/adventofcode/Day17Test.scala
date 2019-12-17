package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike
import org.scalatest.OptionValues._

class Day17Test extends ScalaTestWithActorTestKit with WordSpecLike {

  "Advent of code" must {
    "Day17" should {
      import Day17._
      "lands" should {
        "give back intersection" in {
          val o = OpenSpace
          val S = Scaffold
          val land=Land(Vector(
            Vector(o,o,S,o,o,o,o,o,o,o,o,o,o),
            Vector(o,o,S,o,o,o,o,o,o,o,o,o,o),
            Vector(S,S,S,S,S,S,S,o,o,o,S,S,S),
            Vector(S,o,S,o,o,o,S,o,o,o,S,o,S),
            Vector(S,S,S,S,S,S,S,S,S,S,S,S,S),
            Vector(o,o,S,o,o,o,S,o,o,o,S,o,o),
            Vector(o,o,S,S,S,S,S,o,o,o,S,o,o),
          ))
          land.intersections().map{case(x,y) => x*y}.sum shouldBe 76
        }
        "possible to be created from String" in {
          val landString =
            """..#..........
              |..#..........
              |#######...###
              |#.#...#...#.#
              |#############
              |..#...#...#..
              |..#####...^..""".stripMargin
          val land = Land(landString)
          land.intersections().map{case(x,y) => x*y}.sum shouldBe 76
          land.botPosition().value shouldBe (BotUp,(10,6))
        }
      }
      "paths" should {
        "be detected from land" in {
          val landString =
            """...###
              |...#.#
              |######
              |#..#..
              |#..<..""".stripMargin
          val land = Land(landString)
          val M = MoveInstruction
          val L = TurnLeft
          val R = TurnRight
          fullPath(land) shouldBe List(M(R,4),M(R,2),M(R,2),M(R,5),M(L,2))
        }
      }
      "Part1" should {
        "work with the provided input file" in {
          val listenProbe = createTestProbe[ListenActor.Response]()
          val code = fileToCode()
          val driverBotActor = spawn(AftScaffoldingControlActor(code, listenProbe.ref))
          val response1 = listenProbe.expectMessageType[ListenActor.Response]
          response1.value shouldBe 8520 // Part1
        }
      }
      "Part2" should {
        "work with the provided input file" in {
          val listenProbe = createTestProbe[ListenActor.Response]()
          val code = fileToCode().change(0,2)
          val driverBotActor = spawn(AftScaffoldingControlActor(code, listenProbe.ref))
          val response1 = listenProbe.expectMessageType[ListenActor.Response]
          response1.value shouldBe 8520 // Part1
          val response2 = listenProbe.expectMessageType[ListenActor.Response]
          response2.value shouldBe 42L // Part2
        }
      }
    }
  }
}

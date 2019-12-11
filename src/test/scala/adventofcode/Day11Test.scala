package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike

class Day11Test extends ScalaTestWithActorTestKit with WordSpecLike {

  "Advent of code" must {
    "Day11" should {
      import Day11._
      "Part1" should {
        "work with the provided input file PART1" in {
          val listenProbe = createTestProbe[ListenActor.Response]()
          val code = fileToCode()
          val DriverBotActor = spawn(DriveBotActor(code, listenProbe.ref))
          val response = listenProbe.expectMessageType[ListenActor.Response]
          response.panelCount should not be 54 // too low
          response.panelCount should not be 5181 // too high
          response.panelCount should not be 1317 // too low
          response.panelCount should not be 1318 // too low
          response.panelCount shouldBe 2319
        }
        "work with the provided input file PART2" in {
          val listenProbe = createTestProbe[ListenActor.Response]()
          val code = fileToCode()
          val DriverBotActor = spawn(DriveBotActor(code, listenProbe.ref, startBlack = false))
          val response = listenProbe.expectMessageType[ListenActor.Response]
          response.panelCount shouldBe 100 // result = UERPRFGJ is shown on the final area
        }
      }
    }
  }
}

package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike

class Day13Test extends ScalaTestWithActorTestKit with WordSpecLike {

  "Advent of code" must {
    "Day13" should {
      "Part1" should {
        import Day13._
        import Day13.Part1._
        "work with the provided input file" in {
          val listenProbe = createTestProbe[ListenActor.Response]()
          val code = fileToCode()
          val DriverBotActor = spawn(DrawBotActor(code, listenProbe.ref))
          val response = listenProbe.expectMessageType[ListenActor.Response]
          response.panelCount shouldBe 320
        }
      }
      "Part2" should {
        import Day13._
        import Day13.Part2._
        "be playable the provided input file" in {
          val listenProbe = createTestProbe[ListenActor.Response]()
          val code = fileToCode().change(0,2)
          val DriverBotActor = spawn(DrawBotActor(code, listenProbe.ref))
          val response = listenProbe.expectMessageType[ListenActor.Response]
          response.panelCount shouldBe 15156
        }
      }
    }
  }
}

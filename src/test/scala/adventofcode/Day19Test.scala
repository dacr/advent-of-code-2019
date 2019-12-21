package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike
import org.scalatest.OptionValues._

import akka.testkit.TestDuration
import scala.concurrent.duration._

class Day19Test extends ScalaTestWithActorTestKit with WordSpecLike  {

  "Advent of code" must {
    "Day19" should {
      import Day19._
      "work with the provided input file Part1" in {
        val listenProbe = createTestProbe[ListenActor.Response]()
        val code = fileToCode()
        val driverBotActor = spawn(BeamControlActor(code, listenProbe.ref, areaLimit = (50, 50), None))
        listenProbe.within(2.seconds) { // when debugging
          val response2 = listenProbe.expectMessageType[ListenActor.Response]
          response2.value shouldBe 121 // Part2
        }
      }
      "work with the provided input file Part2 simple test" in {
        val listenProbe = createTestProbe[ListenActor.Response]()
        val code = fileToCode()
        val driverBotActor = spawn(BeamControlActor(code, listenProbe.ref, areaLimit = (50, 50), squareSize=Some(3)))
        listenProbe.within(4.seconds) {
          val response = listenProbe.expectMessageType[ListenActor.Response]
          response.value shouldBe 330017 // Part2 --
        }
      }
      "work with the provided input file Part2" in {
        val listenProbe = createTestProbe[ListenActor.Response]()
        val code = fileToCode()
        val driverBotActor = spawn(BeamControlActor(code, listenProbe.ref, areaLimit = (Int.MaxValue, Int.MaxValue), squareSize=Some(100)))
        listenProbe.within(3.minutes) {
          val response = listenProbe.expectMessageType[ListenActor.Response]
          response.value shouldBe 15090773 // Part2
        }
      }
    }
  }
}

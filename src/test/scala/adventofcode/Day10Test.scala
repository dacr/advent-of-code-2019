package adventofcode

import org.scalatest.{Matchers, WordSpec}
import better.files._

class Day10Test extends WordSpec with Matchers {
  "Advent of code" must {
    "Day10" should {
      "Part1" should {
        import Day10._

        "isHidden" should {
          "return false when position is adjacent" in {
            isHidden(stringToArea("##"), 0.5, 0.5, 1.5,0.5) shouldBe false
            isHidden(stringToArea("##"), 1.5, 0.5, 0.5,0.5) shouldBe false
            isHidden(stringToArea("#\n#"), 0.5, 0.5, 0.5,1.5) shouldBe false
            isHidden(stringToArea("#\n#"), 0.5, 1.5, 0.5,0.5) shouldBe false
            isHidden(stringToArea("#.\n.#"), 0.5, 0.5, 1.5,1.5) shouldBe false
            isHidden(stringToArea("#.\n.#"), 1.5, 1.5, 0.5,0.5) shouldBe false
            isHidden(stringToArea(".#\n#."), 1.5, 0.5, 0.5,1.5) shouldBe false
            isHidden(stringToArea(".#\n#."), 0.5, 1.5, 1.5,0.5) shouldBe false
          }
          "return falses when not adjacent and not hidden" in {
            isHidden(stringToArea("#.#"), 0.5, 0.5, 2.5,0.5) shouldBe false
            isHidden(stringToArea("#.#"), 2.5, 0.5, 0.5,0.5) shouldBe false
            isHidden(stringToArea("#\n.\n#"), 0.5, 0.5, 0.5,2.5) shouldBe false
            isHidden(stringToArea("#\n.\n#"), 0.5, 2.5, 0.5,0.5) shouldBe false
          }
          "return true when position is hidden by an other asteroid horizontally" in {
            isHidden(stringToArea("###"), 0.5, 0.5, 2.5, 0.5) shouldBe true
            isHidden(stringToArea("###"), 2.5, 0.5, 0.5, 0.5) shouldBe true
          }
          "return true when position is hidden by an other asteroid vertically" in {
            isHidden(stringToArea("#\n#\n#"), 0.5, 0.5, 0.5, 2.5) shouldBe true
            isHidden(stringToArea("#\n#\n#"), 0.5, 2.5, 0.5, 0.5) shouldBe true
          }
          "return true when position is hidden by an other asteroid diagonally" in {
            isHidden(stringToArea("#..\n.#.\n..#"), 0.5, 0.5, 2.5,2.5) shouldBe true
            isHidden(stringToArea("#..\n.#.\n..#"), 2.5, 2.5, 0.5,0.5) shouldBe true
            isHidden(stringToArea("..#\n.#.\n#.."), 2.5, 0.5, 0.5,2.5) shouldBe true
            isHidden(stringToArea("..#\n.#.\n#.."), 0.5, 2.5, 2.5,0.5) shouldBe true
          }
        }

        "basic 1" in {
          val areaString =
            """...
              |.#.
              |...
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 0
        }
        "basic 2" in {
          val areaString =
            """...
              |...
              |...
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 0
        }
        "basic 3" in {
          val areaString =
            """.
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 0
        }
        "basic 4" in {
          val areaString =
            """.###.
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 2
        }
        "basic 4-1" in {
          resultString(stringToArea(".###.")) shouldBe ".121."
          resultString(stringToArea(".####.")) shouldBe ".1221."
          resultString(stringToArea(".#####.")) shouldBe ".12221."
        }
        "basic 5" in {
          val areaString =
            """..##.....#
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 2
        }
        "basic 6" in {
          val areaString =
            """.
              |.
              |#
              |#
              |.
              |.
              |#""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 2
        }
        "basic 6-1" in {
          resultString(stringToArea(".###.".toCharArray.mkString("\n"))) shouldBe ".121.".toCharArray.mkString("\n")
          resultString(stringToArea(".####.".toCharArray.mkString("\n"))) shouldBe ".1221.".toCharArray.mkString("\n")
          resultString(stringToArea(".#####.".toCharArray.mkString("\n"))) shouldBe ".12221.".toCharArray.mkString("\n")
        }
        "basic 7" in {
          val areaString =
            """#..
              |..#
              |.#.
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 2
        }
        "basic 8" in {
          val areaString =
            """#..
              |..#
              |.#.
              |""".trim.stripMargin
          resultString(stringToArea(areaString)) shouldBe
            """2..
              |..2
              |.2.""".trim.stripMargin
        }
        "basic 9" in {
          val areaString =
            """###
              |.##
              |.#.
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 5
        }
        "basic 10" in {
          val areaString =
            """###
              |.##
              |.#.
              |""".trim.stripMargin
          resultString(stringToArea(areaString)) shouldBe
            """444
              |.55
              |.4.""".stripMargin
        }
        "simple map 0A" in {
          val areaString =
            """#..
              |.#.
              |..#
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 2
        }
        "simple map 0A-1" in {
          val areaString =
            """#..
              |.#.
              |..#
              |""".trim.stripMargin
          resultString(stringToArea(areaString)) shouldBe
            """1..
              |.2.
              |..1""".stripMargin
        }
        "simple map 0A-2" in {
          val areaString =
            """#...
              |.#..
              |..#.
              |...#
              |""".trim.stripMargin
          resultString(stringToArea(areaString)) shouldBe
            """1...
              |.2..
              |..2.
              |...1""".stripMargin
        }
        "simple map 0A-3" in {
          val areaString =
            """#...
              |....
              |.#..
              |....
              |..#.
              |""".trim.stripMargin
          resultString(stringToArea(areaString)) shouldBe
            """1...
              |....
              |.2..
              |....
              |..1.""".stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 2
        }
        "simple map 0B" in {
          val areaString =
            """#..
              |.##
              |..#
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 3
        }
        "simple map 0C" in {
          val areaString =
            """#...
              |.#.#
              |..#.
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 3
        }
        "simple map 0D" in {
          val areaString =
            """#....
              |.#..#
              |..#..
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 3
        }
        "simple map 0E" in {
          val areaString =
            """#..
              |.#.
              |..#
              |.#.
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 3
        }
        "simple map 0F" in {
          val areaString =
            """##.
              |.#.
              |.##
              |.#.
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 4
        }
        "simple map 0F resultsMap" in {
          val areaString =
            """##.
              |.#.
              |.##
              |.#.
              |""".trim.stripMargin
          resultString(stringToArea(areaString)).trim shouldBe
            """43.
              |.4.
              |.44
              |.3.
              |""".stripMargin.trim
        }
        "simple map 1" in {
          val areaString =
            """.#..#
              |.....
              |#####
              |....#
              |...##
              |""".trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 8
        }
        "simple map 1 resultsMap" in {
          val areaString =
            """.#..#
              |.....
              |#####
              |....#
              |...##
              |""".trim.stripMargin
          resultString(stringToArea(areaString)) shouldBe
            """.7..7
              |.....
              |67775
              |....7
              |...87""".trim.stripMargin
        }
        "simple map 2" in {
          val areaString =
            """......#.#.
              |#..#.#....
              |..#######.
              |.#.#.###..
              |.#..#.....
              |..#....#.#
              |#..#....#.
              |.##.#..###
              |##...#..#.
              |.#....####""".stripMargin.trim.stripMargin
          isHidden(stringToArea(areaString), 6.5d, 3.5d ,0.5d,1.5d) shouldBe true
          countVisible(stringToArea(areaString), 0,1) should not be 34
          countVisible(stringToArea(areaString), 5,8) shouldBe 33
          searchBestAsteroid(stringToArea(areaString)) shouldBe 33
        }
        "simple map 3" in {
          val areaString =
            """#.#...#.#.
              |.###....#.
              |.#....#...
              |##.#.#.#.#
              |....#.#.#.
              |.##..###.#
              |..#...##..
              |..##....##
              |......#...
              |.####.###.""".stripMargin.trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 35
        }
        "simple map 4" in {
          val areaString =
            """.#..#..###
              |####.###.#
              |....###.#.
              |..###.##.#
              |##.##.#.#.
              |....###..#
              |..#.#..#.#
              |#..#.#.###
              |.##...##.#
              |.....#.#..""".stripMargin.trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 41
        }
        "simple map 5" in {
          val areaString =
            """.#..##.###...#######
              |##.############..##.
              |.#.######.########.#
              |.###.#######.####.#.
              |#####.##.#.##.###.##
              |..#####..#.#########
              |####################
              |#.####....###.#.#.##
              |##.#################
              |#####.##.###..####..
              |..######..##.#######
              |####.##.####...##..#
              |.#####..#.######.###
              |##...#.##########...
              |#.##########.#######
              |.####.#.###.###.#.##
              |....##.##.###..#####
              |.#.#.###########.###
              |#.#.#.#####.####.###
              |###.##.####.##.#..##""".stripMargin.trim.stripMargin
          searchBestAsteroid(stringToArea(areaString)) shouldBe 210
        }
        "with provided input file" in {
          searchBestAsteroid(fileToArea("data" / "day10" / "input.txt")) shouldBe 263
        }
      }
    }
  }
}

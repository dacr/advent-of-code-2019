package adventofcode

import better.files._


object Day8 {

  object Part1 {

    case class Layers(content: Array[Array[Int]])

    def runWithInputString(input: String, width: Int, height: Int): Int = {
      val layerSize = width * height
      val layers =
        input
          .toCharArray
          .toVector
          .map(ch => ch.toInt - 48)
          .sliding(layerSize, layerSize)
          .map(layer => layer.sliding(width, width).toVector)
          .toVector

      val layerWithFewestZeroDigits = layers.minBy{layer => layer.flatten.count(_ == 0)}
      val flattened = layerWithFewestZeroDigits.flatten
      val result = flattened.count(_ == 1) * flattened.count(_ == 2)
      result
    }

    def runWithInputFile(): Int = {
      val inputFile = "data" / "day8" / "part1" / "input.txt"
      runWithInputString(inputFile.contentAsString, width = 25, height = 6)
    }

  }


  // ========================================================================================

    object Part2 {

      case class Layers(content: Array[Array[Int]])

      def runWithInputString(input: String, width: Int, height: Int): String = {
        val layerSize = width * height
        val layerCount = input.size / layerSize
        val layers =
          input
            .toCharArray
            .toVector
            .map(ch => ch.toInt - 48)
            .sliding(layerSize, layerSize)
            .map(layer => layer.sliding(width, width).toVector)
            .toVector

        // render layers, 0=black="*", 1=white="_", 2=transparent=" "
        def getPixel(y:Int,x:Int):String = {
          val layersPixel =
            (0 until layerCount)
              .map(layer => layers(layer)(y)(x))
              .toList
              .dropWhile(_ == 2)
          layersPixel match {
            case Nil => " "
            case 0::_=> "_"
            case 1::_=> "#"
          }
        }

        (0 until height).map{ y=>
          (0 until width).map { x => getPixel(y,x)}.mkString
        }.mkString("\n")
      }



      def runWithInputFile(): String = {
        val inputFile = "data" / "day8" / "part1" / "input.txt"
        runWithInputString(inputFile.contentAsString, width = 25, height = 6)
      }

    }
}

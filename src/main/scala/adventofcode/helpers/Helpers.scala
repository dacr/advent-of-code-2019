package adventofcode.helpers

import scala.io.Source

trait Helpers {
  def fileContentToStringList(filename: String):List[String] = {
    Source.fromFile(filename).getLines.toList.map(_.trim).filter(_.size>0)
  }

  def stringContentToStringList(content: String):List[String] = {
    content.split("\n").toList.map(_.trim).filter(_.size>0)
  }

}

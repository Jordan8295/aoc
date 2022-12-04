package utils

import java.io.File
import scala.io.Source
import scala.util.Using

object FileReader {
  def readInput(path: String) = Using(Source.fromFile(s"${new File(".").getCanonicalPath}/src/main/scala/${path}/input.txt")) {
    _.getLines.toList
  }.get
}

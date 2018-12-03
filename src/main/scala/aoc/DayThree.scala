package aoc

import scala.util.Try

object DayThree {

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int)

  def parseLine(line: String): Option[Claim] = {
    val LinePattern = "#(\\d)+\\s+@\\s+(\\d)+,(\\d)+:\\s+(\\d)+x(\\d)+".r
    line match {
      case LinePattern(id, left, top, width, height) =>
        Try {
          Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
        }.toOption
      case _ => None
    }
  }

}

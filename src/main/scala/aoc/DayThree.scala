package aoc

import scala.util.Try
import cats.implicits._

import scala.io.Source

object DayThree {

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int)
  case class Coordinate(x: Int, Y: Int)

  val claims =
    Source
      .fromResource("input03")
      .getLines
      .map(_.trim)
      .flatMap(parseLine)
      .toList

  def parseLine(line: String): Option[Claim] = {
    val LinePattern = "#(\\d+)\\s+@\\s+(\\d+),(\\d+):\\s+(\\d+)x(\\d+)".r
    line match {
      case LinePattern(id, left, top, width, height) =>
        Try {
          Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
        }.toOption
      case _ => None
    }
  }

  def threeA() = println(process(claims))

  def process(claims: List[Claim]): Int = {
    claims
      .foldMap(claimToCoordinates)
      .values
      .map(_.length)
      .filter(_ > 1)
      .toList
      .length
  }

  private def claimToCoordinates(claim: Claim): Map[Coordinate, List[Int]] = {
    (for {
      x <- claim.left until claim.left + claim.width
      y <- claim.top until claim.top + claim.height
    } yield Coordinate(x, y) -> List(claim.id)).toMap
  }

  def threeB() = {
    println(findNonOverlappingClaims(claims))
  }

  def findNonOverlappingClaims(claims: List[Claim]): List[Claim] = {
    val fabric = claims.foldMap(claimToCoordinates)
    claims.filter(c => isOnlyOverlappingCoordinate(c, fabric))
  }

  private def isOnlyOverlappingCoordinate(
      claim: Claim,
      fabric: Map[Coordinate, List[Int]]): Boolean = {
    claimToCoordinates(claim).forall {
      case (coord: Coordinate, _: List[Int]) =>
        fabric.get(coord).exists(_.size == 1)
      case _ => false
    }
  }

}

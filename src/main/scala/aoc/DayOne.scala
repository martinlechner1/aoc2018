package aoc

import cats.implicits._
import cats.kernel.Monoid

import scala.annotation.tailrec
import scala.io.Source

object DayOne {

  val numbersList = Source.fromResource("input01").getLines.map(_.trim).map(_.toInt).toList
  // 1a
  def firstPart(): Unit = println(Monoid.combineAll(numbersList))

  def secondPart(): Unit = println(findDuplicateFrequency(Stream.continually(numbersList.toStream).flatten))

  // 1b
  @tailrec
  def findDuplicateFrequency(numbers: Stream[Int], value: Int = 0, seen: Set[Int] = Set.empty): Int = {
    numbers match {
      case head #:: tail =>
        val current = head + value
        if (seen.contains(current)) {
          current
        } else {
          findDuplicateFrequency(tail, current, seen + current)
        }
    }
  }

}

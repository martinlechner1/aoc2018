package aoc

import cats.Semigroupal
import cats.implicits._

import scala.io.Source
import scala.language.postfixOps

object DayTwo {

  val input = Source.fromResource("input02").getLines.map(_.trim).toList

  def twoA() = println(multiplyResults(input.map(processLine)))

  def twoB() = {
    Semigroupal[List]
      .product(input, input)
      .filterNot(isSelf)
      .map(findCommonCharacters _ tupled)
      .sortBy(_.length)(Ordering[Int].reverse)
      .headOption
      .foreach(println)
  }

  private def processLine(s: String): Map[Int, Int] =
    countLetters(s).values.toSet
      .filter(n => n == 2 || n == 3)
      .toList
      .map(n => Map(n -> 1))
      .combineAll

  private def countLetters(s: String): Map[String, Int] =
    s.split("").map(s => Map(s -> 1)).toList.combineAll

  private def multiplyResults(m: List[Map[Int, Int]]): Int =
    m.combineAll.values.toList match {
      case List(a, b) => a * b
    }

  private def findCommonCharacters(s1: String, s2: String): String =
    s1.zip(s2)
      .flatMap {
        case (c1, c2) if c1 == c2 => Some(c1)
        case _                    => None
      }
      .mkString

  private def isSelf(t: (String, String)): Boolean = t._1 == t._2

}

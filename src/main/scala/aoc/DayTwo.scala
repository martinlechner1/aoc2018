package aoc

import cats.implicits._
import scala.io.Source

object DayTwo {
  val input = Source.fromResource("input02").getLines.map(_.trim).toList

  def twoA() = println(multiplyResults(input.map(processLine)))

  def twoB() = crossProduct(input).map(tup => (tup, distance(tup._1, tup._2))).filter {
    case ((_, _), d) if d > 0 => true
    case _ => false
  }.sortBy(tup => tup._2).headOption.foreach(tup => printCommonCharacters(tup._1._1, tup._1._2))

  private def processLine(s: String): Map[Int, Int] =
    countLetters(s).values.toSet.filter(n => n == 2 || n == 3).toList.map(n => Map(n -> 1)).combineAll

  private def countLetters(s: String): Map[String, Int] =
    s.split("").map(s => Map(s -> 1)).toList.combineAll

  private def multiplyResults(m: List[Map[Int, Int]]): Int =
    m.combineAll.values.toList match {
      case List(a, b) => a * b
    }

  private def printCommonCharacters(s1: String, s2: String): Unit =
    s1.zip(s2).foreach {
      case (c1, c2) if c1 == c2 => print(c1)
      case _ => Unit
    }

  private def distance(s1: String, s2: String): Int =
    s1.zip(s2).map {
      case (c1, c2) if c1 == c2 => 0
      case _ => 1
    }.sum

  private def crossProduct(l: List[String]): List[(String, String)] =
    for {
      l1 <- l
      l2 <- l
    } yield (l1, l2)
}

package aoc

import cats.Monoid

import scala.io.Source

object DayFive {

  implicit val polymerCollapseMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String =
      if (x.lastOption.exists(c =>
            y.headOption.exists(c1 => c1.toLower == c.toLower && c1 != c))) {
        x.dropRight(1)
      } else {
        x + y
      }
  }

  def firstPart(polymer: String): Int = collapsePolymer(polymer).length

  def secondPart(polymer: String): Int =
    ('a' to 'z')
      .map(c => collapsePolymer(polymer.filterNot(_.toLower == c)).length)
      .min

  def collapsePolymer(s: String): String =
    Monoid[String].combineAll(s.split("").toList)

  def run() = {
    val polymer = Source.fromResource("input05").mkString.trim()

    println(firstPart(polymer))
    println(secondPart(polymer))
  }

}

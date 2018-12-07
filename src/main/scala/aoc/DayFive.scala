package aoc

import cats.Monoid

import scala.io.Source

object DayFive {

  val text = Source.fromResource("input05").mkString.trim()

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

  def firstPart() = println(collapsePolymer(text).length)

  def secondPart() =
    println(
      ('a' to 'z')
        .map(c => collapsePolymer(text.filterNot(_.toLower == c)).length)
        .min)

  def collapsePolymer(s: String): String =
    Monoid[String].combineAll(s.split("").toList)

}

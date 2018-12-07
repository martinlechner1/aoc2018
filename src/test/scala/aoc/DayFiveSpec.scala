package aoc

import aoc.DayFive._
import org.scalatest.{Matchers, WordSpec}

class DayFiveSpec extends WordSpec with Matchers {
  "Collapse" should {
    "remove pairs" in {
      collapsePolymer("dabAcCaCBAcCcaDA") shouldEqual "dabCBAcaDA"
    }
  }

}

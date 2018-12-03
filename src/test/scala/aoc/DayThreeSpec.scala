package aoc

import org.scalatest.{Matchers, WordSpec}

import DayThree._

class DayThreeSpec extends WordSpec with Matchers {

  "A line " should {
    "be parsed to none if it is not parsable" in {
      parseLine("") shouldEqual None
    }
    "be parsed to the claim if it is parsable" in {
      parseLine("#3 @ 5,5: 2x2") shouldEqual Some(Claim(3, 5, 5, 2, 2))
    }
    "be parsed to correct claim with multiple digits" in {
      parseLine("#34 @ 15,15: 22x22") shouldEqual Some(
        Claim(34, 15, 15, 22, 22))
    }
  }

  "process" should {
    "calculate the correct number of overlapping fields" in {
      val claims =
        List(Claim(1, 1, 3, 4, 4), Claim(2, 3, 1, 4, 4), Claim(3, 5, 5, 2, 2))
      process(claims) shouldEqual 4
    }
  }

}

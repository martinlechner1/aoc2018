package aoc

import org.scalatest.{Matchers, WordSpec}

import DayThree._

class DayThreeSpec extends WordSpec with Matchers {

  "A line " should {
    "be parsed to none if it is not parsable" in {
      parseLine("") shouldEqual None
    }
    "be parsed to the claim if it is parsable" in {
      parseLine("#1 @ 1,3: 4x4") shouldEqual Some(Claim(1, 1, 3, 4, 4))
      parseLine("#2 @ 3,1: 4x4") shouldEqual Some(Claim(2, 3, 1, 4, 4))
      parseLine("#3 @ 5,5: 2x2") shouldEqual Some(Claim(3, 5, 5, 2, 2))
    }
  }

}

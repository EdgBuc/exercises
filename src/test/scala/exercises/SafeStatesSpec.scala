package exercises;

import org.specs2.mutable.SpecificationWithJUnit;

class SafeStatesSpec extends SpecificationWithJUnit {
  "Safe States" should {
    "return None, if there are no safe states" in {
      //example1.png
      SafeStates.safe(1 -> 1) must beNone
    }

    "return safe transitions in sorted order" in {
      //example2.png
      SafeStates.safe(
        0 -> 1,
        0 -> 2,
        1 -> 2,
        1 -> 3,
        2 -> 5,
        3 -> 0,
        4 -> 5
      ) must beSome(Seq(2, 4, 5))

      //example3.png
      SafeStates.safe(
        0 -> 1,
        0 -> 2,
        0 -> 3,
        0 -> 4,
        1 -> 1,
        1 -> 2,
        2 -> 3,
        2 -> 4,
        3 -> 0,
        3 -> 4,
      ) must beSome(Seq(4))
    }
  }
}

package exercises

import org.specs2.mutable.SpecWithJUnit

class DynamicProgrammingTest extends SpecWithJUnit {
  private val implementation = DynamicProgramming // This is for your implementation.
//    private val implementation = Solution_Spoiler // This one works.
//
  test("empty files")(
    source = "",
    target = "",
    expectedResult = "",
  )

  test("identical files")(
    source =
      """A
        |B
        |C""".stripMargin,
    target =
      """A
        |B
        |C""".stripMargin,
    expectedResult =
      """ A
        | B
        | C""".stripMargin,
  )

  test("identical files with empty final line")(
    source =
      """A
        |B
        |C
        |""".stripMargin,
    target =
      """A
        |B
        |C
        |""".stripMargin,
    expectedResult =
      """ A
        | B
        | C
        | """.stripMargin,
  )

  test("replaced file")(
    source =
      """A
        |B
        |C
        |""".stripMargin,
    target =
      """X
        |Y
        |Z
        |""".stripMargin,
    expectedResult =
      """-A
        |-B
        |-C
        |+X
        |+Y
        |+Z
        | """.stripMargin,
  )

  test("multi-word")(
    source =
      """This is some
        |interesting piece
        |of information
        |""".stripMargin,
    target =
      """This is some
        |very interesting piece
        |of information
        |""".stripMargin,
    expectedResult =
      """ This is some
        |-interesting piece
        |+very interesting piece
        | of information
        | """.stripMargin,
  )

  private def test(description: String)(source: String, target: String, expectedResult: String) = {
    description >> {
      val actualResult = implementation(source, target)
      actualResult must be_==(expectedResult) // TODO This is very fragile. Implement smarter comparison?
    }
  }
}

package exercises.balanced.tree

import org.specs2.mutable.SpecWithJUnit

import scala.language.reflectiveCalls

class LookupIndexTest extends SpecWithJUnit {
  test(BinarySearchTree)
  //  test(spoiler.AvlBst)

  // TODO Add your implementation here.
  test(MySearchTree)

  private def test(factory: Factory): Unit = {
    s"${factory.getClass.getSimpleName}" >> {
      val cases = List(
        (1, false) -> factory.fromInts(),
//        (1, true) -> factory.fromInts(1),
//        (1, true) -> factory.fromInts(2, 3, 1, 0),
//        (1, true) -> factory.fromInts(1, 1, 1),
        (2, true) -> factory.fromInts(1, 2, 3, 4, 5, 6, 7),
//        (1, true) -> factory.fromInts(7, 6, 5, 4, 3, 2, 1),
//        (1, false) -> factory.fromInts(10, 5, 7),
//        (10, true) -> factory.fromInts(10, 12, 11),
      )

      cases.indices.foreach { index =>
        s"basic case #$index" >> {
          val ((needle, expectedState), lookup) = cases(index)
          lookup.contains(needle) must be_==(expectedState)
        }
      }

      "compiles" >> {
        1 must be_==(1)
      } // Just to satisfy return type.
    }
  }

  private type Factory = {
    def fromInts(values: Int*): LookupIndex[Int]
  }
}

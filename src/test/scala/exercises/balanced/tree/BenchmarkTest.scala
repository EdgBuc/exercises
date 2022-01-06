package exercises.balanced.tree

import org.specs2.mutable.SpecWithJUnit

import scala.annotation.tailrec
import scala.language.reflectiveCalls
import scala.util.Random

// This is an anecdotal benchmark to test your solution.
class BenchmarkTest extends SpecWithJUnit {
  private val implementations = List[Factory](
    BinarySearchTree,
//    spoiler.AvlBst,
    // TODO Add your implementation here.
    MySearchTree,
  )

  "benchmark" >> {
    test(implementations, 1)
    test(implementations, 10)
    test(implementations, 100)
    test(implementations, 1000)
  }

  private def test(factories: Seq[Factory], size: Int) = {
    val scale = 1000000 // Just to scale operation cost up and make comparing values easier.
    s"size=$size" >> {
      val valuesToStore = uniqueValues(size).toList.sorted // Worst case for basic BST.
      val valuesToFind = Random.shuffle(uniqueValues(scale))

      println("Building trees...")
      val trees = factories.map(f => f.fromInts(valuesToStore: _*))
      println("Scoring trees...")
      val scores = trees.map(t => measured(valuesToFind.foreach(t.contains)))

      println(s"------ Score for size=$size (lower is better) ------")
      factories.indices.foreach { index =>
        println(s"#$index: ${scores(index)}")
      }

      1 must be_==(1)
    }
  }

  private def measured(op: => Unit): Long = {
    val start = System.currentTimeMillis()
    op
    val end = System.currentTimeMillis()
    end - start
  }

  @tailrec
  private def uniqueValues(size: Int, picked: Set[Int] = Set.empty): Set[Int] = {
    if (picked.size >= size) {
      picked
    } else {
      val extras = Array.fill(size - picked.size)(Random.nextInt())
      uniqueValues(size, picked ++ extras)
    }
  }

  private type Factory = {
    def fromInts(values: Int*): LookupIndex[Int]
  }
}

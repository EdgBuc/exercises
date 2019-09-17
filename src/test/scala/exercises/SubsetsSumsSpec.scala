package exercises

import org.specs2.Specification
import org.specs2.concurrent.ExecutionEnv

class SubsetsSumsSpec(implicit ee: ExecutionEnv) extends Specification {

  def is = "SubsetsSumsSpec".title ^
    s2"""
     check SubsetsSumsSpec if:
    `SubsetsSumsSpec.calculateSums` must calculate sums of collection subsets             $e1
    """

  def e1 = {
    SubsetsSums.calculateSums(List(2, 3, 8)) must haveSize(17)
    SubsetsSums.calculateSums(List(1, 2, 3, 4, 5, 6)) must haveSize(22)
    SubsetsSums.calculateSums(List(1, 1, 1, 1, 1)) must haveSize(6)

    //    val biggestList = List.fill(1000) {
    //      Random.nextInt(1000) + 1
    //    }
    //
    //    val calculatedListSize = SubsetsSums.calculateSums(biggestList).size
    //    calculatedListSize must_== 515878
  }
}

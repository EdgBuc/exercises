package exercises

object FruitsIntoBaskets {
  /**
   *
   * Given an array of characters where each character represents a fruit tree,
   * you are given two baskets, and your goal is to put maximum number of fruits in each basket.
   * The only restriction is that each basket can have only one type of fruit.
   *
   * You can start with any tree, but you can’t skip a tree once you have started.
   * You will pick one fruit from each tree until you cannot, i.e.,
   * you will stop when you have to pick from a third fruit type.
   *
   * Write a function to return the maximum number of fruits in both baskets.
   *
   * @param input
   * @return
   */

  // Solution 1, short one
  def apply(input: Array[Char]): Int = {
    if (input.length < 3) {
      return input.length
    }

    var basket1 = input(0)
    var basket2: Option[Char] = None
    var currBasketSize = 1
    var maxBasketSize = 0
    var closestTree = basket1
    var closestTreeIdx = 0

    def updateClosestTree(i: Int, tree: Char): Unit = {
      if (tree != closestTree) {
        closestTree = tree
        closestTreeIdx = i
      }
      currBasketSize += 1
    }

    for (i <- 1 until input.length) {
      val fruitType = input(i)
      if (basket1 == fruitType) {
        updateClosestTree(i, fruitType)
      } else if (basket2.isEmpty || basket2.contains(fruitType)) {
        basket2 = Some(fruitType)
        updateClosestTree(i, fruitType)
      } else {
        maxBasketSize = Math.max(currBasketSize, maxBasketSize)
        basket1 = closestTree
        basket2 = Some(fruitType)
        currBasketSize = (i - closestTreeIdx) + 1
        closestTree = fruitType
        closestTreeIdx = i
      }
    }

    Math.max(currBasketSize, maxBasketSize)
  }
}

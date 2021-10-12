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
    var windowStart = 0;
    var secondFruitPointer = 1;
    var maxAmount = 0;

    for (windowEnd <- 2 until input.length) {
      if (input(windowStart) == input(secondFruitPointer)) {
        secondFruitPointer = windowEnd
      } else if (input(windowStart) != input(windowEnd) && input(secondFruitPointer) != input(windowEnd)) {
        windowStart = windowEnd-1
        secondFruitPointer = windowEnd
      } else {
        maxAmount = Math.max(maxAmount, windowEnd - windowStart+1)
      }
    }
    maxAmount
  }

  //Solution 2, without using mutations
  def apply2(input: Array[Char]): Int = input.foldLeft((0, 0, 0, 0, 0)) { (acc, cursor) =>
    val (basket1Value, basket2Value, maxAmount, counter, previous) = acc
    if (basket1Value == 0) {
      (cursor, basket2Value, Math.max(maxAmount, counter + 1), counter + 1, cursor)
    } else if (basket1Value != cursor && basket2Value == 0) {
      (basket1Value, cursor, Math.max(maxAmount, counter + 1), counter + 1, cursor)
    } else if (basket1Value != cursor && basket2Value != cursor) {
      (previous, cursor, Math.max(maxAmount, 2), 2, cursor)
    } else {
      (basket1Value, basket2Value, Math.max(maxAmount, counter + 1), counter + 1, cursor)
    }
  }._3
}

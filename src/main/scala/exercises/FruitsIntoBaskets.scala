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

  //Solution 1, without using mutations
  def apply(input: Array[Char]): Int = input.foldLeft((0, 0, 0, 0, 0)) { (acc, cursor) =>
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

 // Solution 2, basic as is
  def apply2(input: Array[Char]): Int = {
    var basket1Value = 0;
    var basket2Value = 0;
    var maxAmount = 0;
    var counter = 0;

    for (i <- input.indices) {
      val cursor = input(i)
      if (basket1Value == 0) {
        basket1Value = cursor
      } else if (basket1Value != cursor && basket2Value == 0) {
        basket2Value = cursor
      } else if (basket1Value != cursor && basket2Value != cursor) {
        basket1Value = input(i - 1)
        basket2Value = cursor
        counter = 1
      }

      if (basket1Value == cursor || basket2Value == cursor) {
        counter += 1
        maxAmount = Math.max(maxAmount, counter)
      }
    }
    maxAmount
  }
}

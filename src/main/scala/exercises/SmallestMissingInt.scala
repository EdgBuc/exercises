package exercises

object SmallestMissingInt extends App {

  val data = (1 to 10000000).toArray
  (-100000 to -1).foreach(i => data(-i) = i)
  //  val data = Array(-2, -4, -1, -1, -2, -600)
  //  val data = Array(-4, 4, -1, 1, 2, 6)
  //  val data = Array(3, 4, -1, 1, 2, 6)
  //  val data = Array(1, 1, 1, 1, 1, 1)
  //  val data = Array(1, 1, 1, 1, 1, 1, 2)
  val dataLength = data.length

  for (idx <- data.indices) {
    while (data(idx) != idx && data(idx) != -1) {
      if (data(idx) < 1 || data(idx) > data.length - 1) {
        data(idx) = -1
      } else if (data(idx) == data(data(idx))) {
        data(data(idx)) = -1
      } else {
        swap(data, idx)
      }
    }
  }

  println(getFirsEmpty(data))

  def getFirsEmpty(data: Array[Int]): Int = {
    for (idx <- 1 until data.length) {
      if (data(idx) == -1) {
        return idx;
      }
    }
    data.length
  }

  def swap(arr: Array[Int], idx: Int): Unit = {
    val a = arr(idx)
    arr(idx) ^= arr(a)
    arr(a) ^= arr(idx)
    arr(idx) ^= arr(a)
  }
}

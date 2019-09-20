package exercises

import java.util

import scala.annotation.tailrec
import scala.math.log
import scala.math.pow

object BinaryTreeSymmetrics extends App {

  def isSymmetricArrayIterable(treeArray: Array[Int]): Boolean = {
    var validatableArray = treeArray.tail

    val treeArrayTailLength = validatableArray.length
    if (treeArrayTailLength % 2 != 0) {
      return false
    }

    val levels: Int = (log(treeArrayTailLength) / log(2)).toInt
    for (i <- 1 to levels) {
      val (partToValidate, other) = validatableArray.splitAt(pow(2, i).toInt)
      validatableArray = other
      if (!validateSymmetric(partToValidate)) {
        return false;
      }
    }
    true
  }

  def isSymmetricArrayRecursive(treeArray: Array[Int]): Boolean = {
    val validatableArray = treeArray.tail

    val treeArrayTailLength = validatableArray.length
    if (treeArrayTailLength % 2 != 0) {
      return false
    }

    val levels: Int = (log(treeArrayTailLength) / log(2)).toInt

    @tailrec
    def validate(array: Array[Int], level: Int = 1): Boolean = {
      val (partToValidate, other) = array.splitAt(pow(2, level).toInt)
      validateSymmetric(partToValidate) match {
        case false => false
        case _ if (level < levels) => validate(other, level + 1)
        case _ => true
      }
    }

    validate(validatableArray)
  }

  def validateSymmetric(array: Array[Int]): Boolean = {
    val (leftPart, rigthPart) = array.splitAt(array.length / 2)
    util.Arrays.equals(leftPart, rigthPart.reverse)
  }
}

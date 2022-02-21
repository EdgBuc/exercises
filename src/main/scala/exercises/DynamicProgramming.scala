package exercises

import scala.util.Either

case class Diff(add: String, remove: String)

object DynamicProgramming {
  def apply(source: String, target: String): String = {
    (source, target) match {
      case (s, t) if (s.isEmpty && t.isEmpty) => "";
      case (s, t) if (s == t) =>
        val str = s.split("\n").map(transformToEqlLine).mkString("\n")
        if (s.endsWith("\n")) str + "\n " else str
      case (s, t) => makeDiffString(compare(s.split("\n").toList, t.split("\n").toList))
    }
  }

  def compare(source: List[String], target: List[String]): List[Either[String, Diff]] = {
    (source, target) match {
      case (Nil, Nil) => List.empty
      case (Nil, head :: tail) => Left(transformToAddLine(head)) +: compare(source, tail)
      case (head :: tail, Nil) => Left(transformToDropLine(head)) +: compare(tail, target)
      case (sourceHead :: sourceTail, targetHead :: targetTail) if (sourceHead == targetHead) =>
        Left(transformToEqlLine(sourceHead)) +: compare(sourceTail, targetTail)
      case (sourceHead :: sourceTail, targetHead :: targetTail) =>
        Right(Diff(transformToAddLine(targetHead), transformToDropLine(sourceHead))) +: compare(sourceTail, targetTail)
    }
  }

  def normalizeOutput(comparison: List[Either[String, Diff]]): List[Either[String, Diff]] = {
    comparison match {
      case Nil => List.empty
      case (head@Left(_)) :: tail => head +: normalizeOutput(tail)
      case (head@Right(Diff(add, remove))) :: tail =>
        val normalized = normalizeOutput(tail)
        normalized match {
          case Right(Diff(nAdd, nRemove)) :: tail =>
            Right(Diff(s"$add\n$nAdd", s"$remove\n$nRemove")) +: tail
          case _ => head +: normalized
        }
    }
  }

  def makeDiffString(comparison: List[Either[String, Diff]]): String = {
    normalizeOutput(comparison).map {
      case Left(str) => str
      case Right(Diff(add, remove)) => s"$remove\n$add"
    }.mkString("\n") + "\n "
  }

  def transformToAddLine(string: String): String = s"+$string"

  def transformToDropLine(string: String): String = s"-$string"

  def transformToEqlLine(string: String): String = s" $string"
}

package exercises


import scala.collection.Seq

object SafeStates {
  /*
    There is a directed graph expressed as transitions between nodes.
    A node is a terminal node if there are no outgoing edges.
    A node(state) is a safe node if every possible path starting from that node leads to a terminal node.
    Return an sequence containing all the safe nodes of the graph or None if there are none.
    The answer should be sorted in ascending order.
  */
  def safe(firstTransition: (Int, Int), transitions: (Int, Int)*): Option[Seq[Int]] = {
    val referencesMap = (firstTransition :: List.from(transitions)).foldLeft(Map.empty[Int, List[Int]]) {
      case (map, tran) =>
        val references = tran._1 :: map.getOrElse(tran._2, List.empty)
        val updatedMap = map + (tran._2 -> references)
        if (updatedMap.contains(tran._1)) {
          updatedMap
        } else {
          updatedMap + (tran._1 -> List.empty)
        }
    }

    val referencesAmountMap = (firstTransition :: List.from(transitions)).foldLeft(Map.empty[Int, Int]) {
      case (map, tran) =>
        val amountOfRefs = map.getOrElse(tran._1, 0) + 1
        val updatedMap = map + (tran._1 -> amountOfRefs)
        if (updatedMap.contains(tran._2)) {
          updatedMap
        } else {
          updatedMap + (tran._2 -> 0)
        }
    }
    println(referencesMap)
    println(referencesAmountMap)
    val safeNodes = sort(referencesMap, referencesAmountMap)
    println(safeNodes)
    if (safeNodes.isEmpty) {
      None
    } else {
      Some(safeNodes)
    }
  }

  def sort(referencedByMap: Map[Int, List[Int]], referencesAmountMap: Map[Int, Int]): Seq[Int] = {
    val terminalNodes = referencesAmountMap.filter(elm => elm._2 == 0).keys
    if (terminalNodes.isEmpty) {
      Seq.empty
    } else {
      val updatedReferencesAmountMap = terminalNodes
        .flatMap(node => referencedByMap.getOrElse(node, Set.empty))
        .foldLeft(referencesAmountMap)((map, reference) => map + (reference -> (map(reference) - 1)))

      val cleanMaps = terminalNodes.foldLeft((referencedByMap, updatedReferencesAmountMap)) {
        case ((refMap, refAmountMap), node) => (refMap - node, refAmountMap - node)
      }
      sort(cleanMaps._1, cleanMaps._2) :++ terminalNodes.toList
    }
  }

}

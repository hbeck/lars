package lars.graph.quotient

import lars.graph.util.{HasEdge, Add}
import lars.graph.{Graph, InOutGraph}

/**
 * the condensation of a strongly connected graph is the quotient graph where
 * the strongly connected components form the blocks of the partition.
 *
 * Created by hb on 7/10/15.
 */
case class Condensation[V](override val adjList:Map[V,Set[V]]) extends InOutGraph[V](adjList)

object Condensation {

 //TODO factor out sccs by a generic function towards quotient
  def apply[V,G <: Graph[V]](g: G, sccs: collection.immutable.Map[V,G]): Condensation[G] = {
    val nodes: Set[G] = sccs.values.toSet
    var adjList = Map[G,Set[G]]()
    for (n <- nodes) {
      adjList = adjList + (n -> Set[G]())
    }
    for (e <- g.edges) {
      val fromC = sccs(e._1)
      val toC = sccs(e._2)
      if (fromC != toC && !HasEdge(adjList,fromC,toC)) {
        adjList = Add(adjList,fromC,toC)
      }
    }
    new Condensation(adjList)
  }

}

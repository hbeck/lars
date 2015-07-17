package lars.graph.quotient

import lars.graph.{DiGraph, Graph}

/**
 * https://en.wikipedia.org/wiki/Quotient_graph
 *
 * Created by hb on 7/16/15.
 */
case class QuotientGraph[V](override val adjList:Map[V,Set[V]]) extends DiGraph[V](adjList)

object QuotientGraph {

  def apply[V,G <: Graph[V]](g: G, sccs: collection.immutable.Map[V,G]): Unit = { //TODO QuotientGraph = {
  }
}

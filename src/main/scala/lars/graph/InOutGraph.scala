package lars.graph

import lars.graph.traits.Incoming

/**
 * graph based on adjacency list for both incoming and outgoing directions
 *
 * Created by hb on 7/16/15.
 */
class InOutGraph[V](override val adjList:Map[V,Set[V]]) extends BasicGraphImpl[V](adjList) with Incoming[V] {

  private var in: Map[V, Set[V]] = Map[V, Set[V]]()

  for (n <- nodes) {
    in = in + (n -> Set[V]())
  }
  for (from <- nodes) {
    val toNodes: Set[V] = super.outgoing(from)
    for (to <- toNodes) {
      val set = in.getOrElse(to,Set()) + from
      in = in.updated(to,set)
    }
  }

  def incoming(e: V) = in(e)

}

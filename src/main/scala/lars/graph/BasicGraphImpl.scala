package lars.graph

import lars.graph.traits.BasicGraph

/**
 * graph based on adjacency list
 *
 * Created by hb on 7/16/15.
 */
class BasicGraphImpl[V](val adjList:Map[V,Set[V]]) extends Graph[V] with BasicGraph[V] {

  override def outgoing(n: V) = adjList(n)

  override def hasEdge(n:V, m:V) : Boolean = {
    outgoing(n).contains(m)
  }

  override def isLeaf(n: V) = outgoing(n).isEmpty
}

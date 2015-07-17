package lars.util.graph

/**
 * graph based on adjacency list
 *
 * Created by hb on 7/16/15.
 */
class SimpleGraph[V](val adjList:Map[V,Set[V]]) extends Graph[V] with AdjList[V] {

  def outgoing(n: V) = adjList(n)

  override def hasEdge(n:V, m:V) : Boolean = {
    outgoing(n).contains(m)
  }

  def isLeaf(n: V) = outgoing(n).isEmpty

}

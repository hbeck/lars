package lars.util.graph

/**
 * Created by hb on 7/16/15.
 */
class InOutGraph[V](val adjList:Map[V,Set[V]]) extends Graph {

  val nodes = adjList.keySet

  private val out = adjList
  private var in: Map[V, Set[V]] = Map[V, Set[V]]()

  for (n <- nodes) {
    in += (n -> Set[V]())
  }
  for (from <- nodes) {
    val toNodes: Set[V] = out(from)
    for (to <- toNodes) {
      in = in.updated(to,in(to)+from)
    }
  }

  def incoming(e: V) = in(e)

  def outgoing(e: V) = out(e)

  def hasEdge(n:V, m:V) : Boolean = {
    in(n).contains(m)
  }

  def isLeaf(n: V) = incoming(n).isEmpty

}

package lars.strat.alg

import lars.strat.DepGraph

/**
 * the nodes of this graph correspond to subgraphs of a dependency graph.
 *
 * an edge (xP,yP) between to nodes of this graph, if there is an edge
 * between x and y (with any dependency) in the original dependency graph,
 * where x is in xP and y in yP
 *
 * Created by hb on 7/16/15.
 */
abstract class PartitionedGraph(val nodes:collection.immutable.Set[DepGraph], val adjList:Map[DepGraph,Set[DepGraph]]) {

  private val out = adjList
  private var in: Map[DepGraph, Set[DepGraph]] = Map[DepGraph, Set[DepGraph]]()

  for (n <- nodes) {
    in += (n -> Set[DepGraph]())
  }
  for (from <- nodes) {
    val toNodes: Set[DepGraph] = out(from)
    for (to <- toNodes) {
      in = in.updated(to,in(to)+from)
    }
  }

  def incoming(e: DepGraph) = in(e)

  def outgoing(e: DepGraph) = out(e)

  def hasEdge(n:DepGraph, m:DepGraph) : Boolean = {
    in(n).contains(m)
  }

}

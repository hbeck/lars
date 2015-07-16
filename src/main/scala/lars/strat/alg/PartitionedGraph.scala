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
abstract class PartitionedGraph(val nodes:collection.immutable.Set[DepGraph]) {

  //TODO this should not be necessarity at this level
  def graph2stratum:Map[DepGraph,Int]

  def maxStratum() = graph2stratum.values.reduce(math.max)

}

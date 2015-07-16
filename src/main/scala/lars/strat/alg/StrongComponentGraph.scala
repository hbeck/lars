package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.DepGraph
import lars.util.graph.{Add, HasEdge}


/**
 * every node is a strongly connected component of the original dependency graph
 *
 * Created by hb on 7/10/15.
 */
class StrongComponentGraph(override val nodes:Set[DepGraph], override val adjList:Map[DepGraph,Set[DepGraph]]) extends PartitionedGraph(nodes,adjList)

object StrongComponentGraph {

  // connect two components fromC and toC with an arc (in this direction),
  // if there is an edge (from,to) in g, where
  // from is in SCC fromC and to is in a different SCC toC
  // thus, ignore a further distinction between >= (geq) and > (grt) towards
  // a 'maximal' stratification (using longest possible paths)
  // alg:
  //   for all edges (from,to) in g:
  //     get SCCs fromC, toC
  //     if fromC == toC continue //dep in edge can be >= or =
  //     if there exists already an edge between fromC, toC continue
  //     if e(from,to,dep) (dep in {>=,>})
  //       create edge (fromC,toC)
  //
  // result: DAG, not necessarily (weakly) connected
  def apply(depGraph:DepGraph, sccs: collection.immutable.Map[ExtendedAtom,DepGraph]): StrongComponentGraph = {
    var adjList = Map[DepGraph,Set[DepGraph]]()
    for (e <- depGraph.edges) {
      val fromC = sccs(e.from)
      val toC = sccs(e.to)
      if (fromC != toC && !HasEdge(adjList,fromC,toC)) {
        adjList = Add(adjList,fromC,toC)
      }
    }
    new StrongComponentGraph(sccs.values.toSet,adjList)
  }

}

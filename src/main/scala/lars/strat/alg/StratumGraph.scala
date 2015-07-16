package lars.strat.alg

import lars.strat.DepGraph

/**
 * a node in this graph corresponds to a strong component in the original dependency graph plus
 * edges with dependency `geq`
 *
 * Created by hb on 7/16/15.
 */
class StratumGraph(override val nodes:Set[DepGraph]) extends PartitionedGraph(nodes) {

  override def graph2stratum: Map[DepGraph, Int] = Map[DepGraph,Int]()
}

object StratumGraph {
  def apply(scg: StrongComponentGraph): Unit = { //StratumGraph = {
    //TODO create by extendening the nodes of scg:
    //for every extended atom x (=node of original graph) in the a node X of scg,
    //add edge (x,y) to X if the dependency of (x,y) is `geq` - rethink
  }
}

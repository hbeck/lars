package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.DepGraph
import lars.util.graph.InOutGraph

/**
 * contains as nodes the subgraphs of the dependency graph whose nodes get the same stratum number.
 *
 * similar like StrongComponentDAG, but adds (transitively) the nodes which are reachable with `geq` edges
 * 
 * Created by hb on 7/16/15.
 */
case class StratumDAG(override val adjList:Map[DepGraph,Set[DepGraph]]) extends InOutGraph[DepGraph](adjList)

object StratumDAG {

  def apply(depGraph:DepGraph, sccs: collection.immutable.Map[ExtendedAtom,DepGraph]): StratumDAG = {
    var nodes = Set[DepGraph]()
    var adjList = Map[DepGraph,Set[DepGraph]]()

    //TODO

    new StratumDAG(adjList)
  }
}

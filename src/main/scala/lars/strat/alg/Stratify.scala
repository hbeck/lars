package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.Program
import lars.strat.{DepGraph, Strat, grt}

import scala.collection.mutable.{Set, HashSet}

/**
 * Created by hb on 7/10/15.
 */
object Stratify extends (Program => Option[Strat]) {

  /**
   * @return a Stream Stratification (SStrat) for program P, if it has one
   */
  def apply(P: Program): Option[Strat] = {

    // dependency graph g
    val depGraph = DepGraph(P)

    // determine strongly connected components (SCCs)
    val sccs: Map[ExtendedAtom,DepGraph] = SCCs(depGraph)

    // if any of these components contains an edge with dependency > (greater),
    // no stratification exists
    for (g <- sccs.values) {
      for (e <- g.edges) {
        if (e.dep == grt) {
          return None
        }
      }
    }

    // else, view resulting component graph, consisting of the SCCs as nodes
    // edges between fromC and toC, if {from,to}C is are different components
    // of the nodes from/to with a dependency in {>=,>}
    val cg = ComponentGraph(depGraph,sccs)

    // for every component of the DAG, assign stratum index
    cg.assignStratumNumbers()

    val nodesInStratum = new collection.mutable.HashMap[Int,Set[ExtendedAtom]]()
    for (i <- 0 to cg.maxStratum()) {
      nodesInStratum += i -> new HashSet[ExtendedAtom]()
    }
    val graphStratum = cg.graph2stratum
    val graphs = graphStratum.keys
    for (g <- graphs) {
      val stratum = graphStratum(g)
      nodesInStratum(stratum) ++= g.nodes()
    }

    val m = nodesInStratum.toMap
    Option(Strat(m))
  }

}
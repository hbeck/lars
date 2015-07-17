package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.graph.DiGraph
import lars.graph.alg.{BottomUpNumbering, SCCs}
import lars.graph.quotient.Condensation

/**
 * Created by hb on 7/10/15.
 *
 * note: we can have different stratifications and study the impact on performance later
 */
object Stratify {

  /**
   * @return a Stream Stratification (SStrat) for program P, if it has one
   */
  def apply(P: StdProgram): Option[Stratification] = {
    apply(DepGraph(P))
  }

  def apply(depGraph: DepGraph) : Option[Stratification] = {

    // determine strongly connected components (SCCs)
    val sccs: Map[ExtendedAtom,DiGraph[ExtendedAtom]] = SCCs[ExtendedAtom](depGraph)

    // if any of these components contains an edge with dependency > (greater),
    // no stratification exists
    for (g <- sccs.values) {
      val depGraph = g.asInstanceOf[DepGraph]
      for ((from,to) <- depGraph.edges) {
        if (depGraph.label(from,to) == grt) {
          return None
        }
      }
    }

    // TODO use instead a QuotientGraph with a relaxed partitioning criterion
    // that includes after the strongly connected components those arcs reachable with geq
    val cnd: Condensation[DiGraph[ExtendedAtom]] = Condensation(depGraph,sccs)

    val subgraphNr: Map[DiGraph[ExtendedAtom], Int] = BottomUpNumbering(cnd)

    val nrToAtoms: Map[Int, Set[ExtendedAtom]] = createStratumMapping(subgraphNr)

    Option(Stratification(nrToAtoms))
  }
  
  def createStratumMapping(subgraphNr:Map[DiGraph[ExtendedAtom],Int]): Map[Int, Set[ExtendedAtom]] = {
    var m = Map[Int,Set[ExtendedAtom]]()
    for ((graph,nr) <- subgraphNr) {
      if (m.contains(nr)) {
        val set = m(nr) ++ graph.nodes
        m = m.updated(nr,set)
      } else {
        m = m.updated(nr,graph.nodes)
      }
    }
    m
  }

}

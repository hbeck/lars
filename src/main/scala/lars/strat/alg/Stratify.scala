package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.graph.alg.{SCCs, BottomUpNumbering}
import lars.graph.quotient.Condensation
import lars.strat.{grt, DepGraph, Stratification}

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
    val sccs: Map[ExtendedAtom,DepGraph] = SCCs(depGraph)

    // if any of these components contains an edge with dependency > (greater),
    // no stratification exists
    for (g <- sccs.values) {
      for (e <- g.depEdges) {
        if (e.dep == grt) {
          return None
        }
      }
    }

    val scg:Condensation[DepGraph] = Condensation(depGraph,sccs)
    // TODO use instead a StratumAG

    val subgraphNr: Map[DepGraph,Int] = BottomUpNumbering(scg)

    val nrToAtoms: Map[Int, Set[ExtendedAtom]] = createStratumMapping(subgraphNr)

    Option(Stratification(nrToAtoms))
  }
  
  def createStratumMapping(subgraphNr:Map[DepGraph,Int]): Map[Int, Set[ExtendedAtom]] = {
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

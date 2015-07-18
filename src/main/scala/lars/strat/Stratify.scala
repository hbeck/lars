package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.graph.alg.BottomUpNumbering
import lars.graph.quotient.{Condensation, QuotientGraph}

/**
 * Created by hb on 7/10/15.
 *
  */
object Stratify {

  /**
   * @return a Stream Stratification (SStrat) for program P, if it has one
   */
  def apply(P: StdProgram): Option[Stratification] = {
    apply(DepGraph(P))
  }

  def apply(depGraph: DepGraph) : Option[Stratification] = {

    val condensation: QuotientGraph[ExtendedAtom] = Condensation(depGraph)

    // if any of these components contains an edge with dependency > (greater),
    // no stratification exists
    if (hasCycleWithGrt(depGraph, condensation)) {
      return None
    }

    //TODO use different quotient graph
    val subgraphNr: Map[Set[ExtendedAtom], Int] = BottomUpNumbering(condensation)

    val nrToAtoms: Map[Int, Set[ExtendedAtom]] = createStratumMapping(subgraphNr)

    Option(Stratification(nrToAtoms))
  }

  def hasCycleWithGrt(depGraph: DepGraph, condensation: QuotientGraph[ExtendedAtom]): Boolean = {
    for (scc <- condensation.nodes) {
      val dg = depGraph.subgraph(scc)
      if (dg.edges.exists{ e => dg.label(e._1,e._2) == grt }) {
        return true
      }
    }
    return false
  }
  
  def createStratumMapping(subgraphNr:Map[Set[ExtendedAtom],Int]): Map[Int, Set[ExtendedAtom]] = {
    var m = Map[Int,Set[ExtendedAtom]]()
    for ((nodes,nr) <- subgraphNr) {
      if (m.contains(nr)) {
        val set = m(nr) ++ nodes
        m = m.updated(nr,set)
      } else {
        m = m.updated(nr,nodes)
      }
    }
    m
  }

}

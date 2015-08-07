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

    val stratumG: QuotientGraph[ExtendedAtom] = StratumGraph(depGraph)

    /*
    TODO
    further step demanded, where the condensation is 'minimalized' by edges with dependency geq (>=)
    however, a naive local expansion of blocks is not possible, this may lead to cycles with remaining grt (>)
    in general. intuitively, we want to add two blocks A and B whenever there is no path connecting the two with
    and edge labeled with > (in the original graph)
    */
    //val stratG = StratumGraph(condensation)

    val subgraphNr: Map[Set[ExtendedAtom], Int] = BottomUpNumbering(stratumG)

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
    false
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

package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.strat.{DepGraph, Stratification, grt}
import lars.util.graph.BottomUpNumbering

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
      for (e <- g.edges) {
        if (e.dep == grt) {
          return None
        }
      }
    }

    // else, view resulting component graph, consisting of the SCCs as nodes
    // edges between fromC and toC, if {from,to}C is are different components
    // of the nodes from/to with a dependency in {>=,>};
    // for every component of the DAG, assign stratum index
    val cg = StrongComponentGraph(depGraph,sccs) //TODO instead StratumGraph which is "minimal"

    val subgraphNr: Map[DepGraph,Int] = BottomUpNumbering(cg)

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

package lars.strat


import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.Program

import scala.collection.mutable.Set


/**
 * Stream Stratification
 * Created by hb on 7/7/15.
 */
case class SStrat(map: Map[Int,Set[ExtendedAtom]])

object SStrat {
  /**
   * @return a Stream Stratification (SStrat) for program P, if it has one
   */
  def create(P: Program): Option[SStrat] = {

    // dependency graph g

    // determine strongly connected components (SCCs)

    // if any of these components contains an edge with dependency > (greater),
    // no stratification exists

    // else, view resulting component graph, consisting of the SCCs as nodes;
    // no edges so far

    // connect two components fromC and toC with an arc (in this direction),
    // if there is an edge (from,to) in g, where
    // from is in SCC fromC and to is in a different SCC toC
    // thus, ignore a further distinction between >= (geq) and > (grt) towards
    // a 'maximal' stratification (using longest possible paths)
    // alg:
    //   for all edges (from,to) in g:
    //     get SCCs fromC, toC
    //     if fromC == toC continue //dep in edge can be >= or =
    //     if there exists an edge between fromC, toC continue
    //     if e(from,to,dep) (dep in {>=,>})
    //       create edge (fromC,toC)
    //     else
    //       create edge (toC,fromC)
    //
    // result: DAG, not necessarily (weakly) connected

    // for every component of the DAG, assign stratum index
    // alg:
    //   select random node, give it index 0
    //   walk the component BFS in all directions with increasing/decreasing numbers:
    //   given the current node n has index i, assign
    //   - index i+1 for neighbors m where e(m,n), and
    //   - index i-1 for neighbors m where e(n,m)

    // normalize every component s.t. lowest index in every component is 0

    None
  }
}
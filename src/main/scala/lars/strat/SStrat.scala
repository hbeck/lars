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
    /*
     1. create SCCs
     2. if any of those contains >, return None
     3. sort SCCs by <, <=, uncomparable (?)
     4. merge
     try with fig4 first - unrelated nodes?
     */
    //TODO
    None
  }
}
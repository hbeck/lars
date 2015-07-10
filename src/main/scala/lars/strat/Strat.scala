package lars.strat


import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.Program
import lars.strat.alg.Stratify

import scala.collection.mutable.Set


/**
 * Stream Stratification
 * Created by hb on 7/7/15.
 */
case class Strat(map: Map[Int,Set[ExtendedAtom]])

object Strat {
  def apply(P:Program): Option[Strat] = Stratify(P)
}
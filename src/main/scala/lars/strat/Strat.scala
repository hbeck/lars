package lars.strat


import lars.core.MapUtils
import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.Program
import lars.strat.alg.Stratify

import scala.collection.mutable.Set


/**
 * Stream Stratification
 * Created by hb on 7/7/15.
 */
case class Strat(map: Map[Int,Set[ExtendedAtom]]) {

  val maxStratum = map.keySet.reduce(math.max)
  private val stratum: Map[ExtendedAtom,Int] = MapUtils.reverse(map)

  def apply(stratum:Int): Set[ExtendedAtom] = map(stratum)
  def apply(x:ExtendedAtom): Int =  stratum(x)
}

object Strat {
  
  def apply(P:Program): Option[Strat] = Stratify(P)
  
  def isStratification(strat: Map[ExtendedAtom,Int], P: Program): Boolean = {
    val G = DepGraph(P)
    for (e <- G.edges) {
      e.dep match {
        case geq => if (strat(e.from) < strat(e.to)) return false
        case grt => if (strat(e.from) <= strat(e.to)) return false
        case eql => if (strat(e.from) != strat(e.to)) return false
      }
    }
    return true
  }
}
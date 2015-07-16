package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.strat.alg.Stratify
import lars.util.map.ReverseFromSet

/**
 * Stream Stratification
 * Created by hb on 7/7/15.
 */
case class Stratification(map: Map[Int,Set[ExtendedAtom]]) { //"other" way as defined due to algorithmic reasons; apply method for other direction provided

  val maxStratum = map.keySet.reduce(math.max)

  private val stratumNr: Map[ExtendedAtom,Int] = ReverseFromSet(map)

  def apply(i:Int): Set[ExtendedAtom] = map(i)
  def apply(x:ExtendedAtom): Int =  stratumNr(x)
}

object Stratification {
  
  def apply(P: StdProgram): Option[Stratification] = Stratify(P)
  
  def isStratification(strat: Map[ExtendedAtom,Int], P: StdProgram): Boolean = {
    val G = DepGraph(P)
    for (e <- G.edges) {
      e.dep match {
        case `geq` => if (strat(e.from) < strat(e.to)) return false
        case `grt` => if (strat(e.from) <= strat(e.to)) return false
        case `eql` => if (strat(e.from) != strat(e.to)) return false
      }
    }
    return true
  }
}
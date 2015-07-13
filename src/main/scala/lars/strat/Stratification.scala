package lars.strat

import lars.core.MapUtils
import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.Program
import lars.strat.alg.{ComponentGraph, Stratify}

/**
 * Stream Stratification
 * Created by hb on 7/7/15.
 */
case class Stratification(map: Map[Int,Set[ExtendedAtom]]) { //"other" way as defined due to algorithmic reasons; apply method for other direction provided

  val maxStratum = map.keySet.reduce(math.max)

  private val stratumNr: Map[ExtendedAtom,Int] = MapUtils.reverse(map)

  def apply(i:Int): Set[ExtendedAtom] = map(i)
  def apply(x:ExtendedAtom): Int =  stratumNr(x)
}

object Stratification {
  
  def apply(P:Program): Option[Stratification] = Stratify(P)

  def apply(cg:ComponentGraph) : Stratification = {
    val nodesInStratum = new collection.mutable.HashMap[Int,Set[ExtendedAtom]]()
    for (i <- 0 to cg.maxStratum()) {
      nodesInStratum += i -> Set[ExtendedAtom]()
    }
    val graphStratum = cg.graph2stratum
    val graphs = graphStratum.keys
    for (g <- graphs) {
      val stratum = graphStratum(g)
      nodesInStratum(stratum) ++= g.nodes //! immutable
    }

    Stratification(nodesInStratum.toMap)
  }
  
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
package lars.strat

import lars.core.semantics.programs.AS
import lars.core.semantics.programs.general.GeneralProgram
import lars.core.semantics.streams.S

import scala.collection.mutable

/**
 * Created by hb on 7/14/15.
 */
object IAS {

  def apply(P:GeneralProgram, D: S, t: Int): Set[S] = {
    val p: Map[Int, GeneralProgram] = Strata(P)
    val I = new mutable.HashMap[Int,Set[S]]()
    I(0) = AS(p(0),D,t)
    val n = p.size-1
    for (k <- 1 to n) {
      I(k) = I(k-1).flatMap( i => AS(p(k),i,t) )
    }
    I(n)
  }



}

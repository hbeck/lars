package lars.strat

import lars.core.semantics.programs.{AS, Program}
import lars.core.semantics.streams.S

import scala.collection.mutable

/**
 * Created by hb on 7/14/15.
 */
object IAS {

  def apply(P:Program, D: S, t: Int): Set[S] = {
    val p: Map[Int, Program] = Strata(P)
    val I = new mutable.HashMap[Int,Set[S]]()
    I(0) = AS(p(0),D,t)
    val n = p.size-1
    for (j <- 1 to n) {
      I(j) = I(j-1).flatMap( i => AS(p(j),i,t) )
    }
    I(n)
  }



}

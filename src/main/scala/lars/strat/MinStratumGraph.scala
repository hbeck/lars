package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.quotient.{Block, QuotientGraph}

/**
 * Created by hb on 8/11/15.
 */
object MinStratumGraph {

  def apply(G: DepGraph, quotientGraph: QuotientGraph[ExtendedAtom]): QuotientGraph[ExtendedAtom] = {



  }

//  def canMerge(G: DepGraph, b1:Block[ExtendedAtom], b2:Block[ExtendedAtom]): Boolean = {
//    for (x <- b1) {
//      for (y <- b2) {
//        if (hasPathWithGrt(G,x,y)) {
//          return false
//        }
//      }
//    }
//  }

}

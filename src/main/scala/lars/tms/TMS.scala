package lars.tms

import lars.core.semantics.formulas._
import lars.core.semantics.programs.standard.{StdRule, StdProgram}
import lars.tms.status.Labels
import lars.tms.status.rule.fVal

/**
 * Created by hb on 6/25/15.
 */
case class TMS(N:Set[ExtendedAtom],J:Set[J]) {

}

// Missing labels for J?
object TMS {
  def apply(P: StdProgram, L:Labels): TMS = {
    var j = Set[J]()
    var nSet = Set[ExtendedAtom]()
    for(rule <- P.rules){
      if(fVal(L,rule)) {
        j += new J(rule.Bp, rule.Bn, rule.h)
      }
      nSet += rule.h
    }
    TMS(nSet,j)
  }
}

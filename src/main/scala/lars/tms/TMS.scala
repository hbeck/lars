package lars.tms

import lars.core.semantics.formulas.{Not, Formula, ExtendedAtom}
import lars.core.semantics.programs.standard.{StdRule, StdProgram}
import lars.tms.status.Labels

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
      var I = Set[ExtendedAtom]()
      var O = Set[ExtendedAtom]()
/*      rule.body match {
        case Not(fm:ExtendedAtom) =>
      }*/
      println(rule.body)
      j += new J(Set(),Set(),rule.h)
      nSet += rule.h
      val io:Formula = rule.body
    }
    TMS(nSet,j)
  }
}

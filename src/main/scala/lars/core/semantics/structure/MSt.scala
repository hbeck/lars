package lars.core.semantics.structure

import lars.core.semantics.formulas._
import lars.core.semantics.streams.S

/**
 * Created by hb on 5/26/15.
 */
//M,S,t
case class MSt(m: M, s: S, t: Int) {

  //entailment
  def ||- (fm: Formula) : Boolean = {

    val T = s.T
    val v:Map[Int,Set[Atom]] = m.v.mapping

    fm match {
      case a: Atom           => v.getOrElse(t,Set()).contains(a) || m.B.contains(a)
      case Not(fm1)          => !(this ||- fm1)
      case And(fm1, fm2)     => (this ||- fm1) && (this ||- fm2)
      case Or(fm1, fm2)      => (this ||- fm1) || (this ||- fm2)
      case Implies(fm1, fm2) => !(this ||- fm1) || (this ||- fm2)
      case D(fm1)            => T.timePoints exists { m/s/_ ||- fm1 } //TODO see Timeline definition
      case B(fm1)            => T.timePoints forall { m/s/_ ||- fm1 }
      case At(u,fm1)         => (T contains u) && (m/s/u ||- fm1)
      case W(w,ch,x,fm1)     => {
        val s0 = S(m.T,m.v)
        val s1 = w(ch(s0,s),t,x)
        m/s1/t ||- fm1
      }

    }
  }
}

package lars.core.semantics.formulas

import WindowOperators.StreamChoice
import lars.core.windowfn.{WindowFunction, WindowParameters}

/**
 * Created by hb on 5/26/15.
 */
abstract class Formula {
  def and(fm: Formula): Formula = And(this,fm)
  def or(fm: Formula): Formula = Or(this, fm)
  def implies(fm: Formula): Formula = Implies(this, fm)
}

case class Atom(s: String*) extends Formula {
  override def equals(that:Any): Boolean =
    that match {
      case that: Atom => that.canEqual(this) && this.s.sameElements(that.asInstanceOf[Atom].s)
      case _ => false
    }
}

case class Not(fm: Formula) extends Formula
case class And(fm1: Formula, fm2: Formula) extends Formula
case class Or(fm1: Formula, fm2: Formula) extends Formula
case class Implies(fm1: Formula, fm2: Formula) extends Formula
case class D(fm: Formula) extends Formula
case class B(fm: Formula) extends Formula
case class At(t: Int, fm: Formula) extends Formula
case class W[X <: WindowParameters](wfn: WindowFunction[X], ch: StreamChoice, x: X, fm: Formula) extends Formula
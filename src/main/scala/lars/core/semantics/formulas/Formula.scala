package lars.core.semantics.formulas

import lars.core.semantics.formulas.WindowOperators.StreamChoice
import lars.core.windowfn.{WindowFunction, WindowParameters}

/**
 * Created by hb on 5/26/15.
 */
abstract class Formula {
  def and(fm: Formula): Formula = And(this,fm)
  def or(fm: Formula): Formula = Or(this, fm)
  def implies(fm: Formula): Formula = Implies(this, fm)
}

case class WindowOperator[X <: WindowParameters](wfn: WindowFunction[X], ch:StreamChoice, x:X)

abstract class ExtendedAtom extends Formula

//TODO unify 'formula ontology' s.t. is doesnt need recasting later
case class DiamAtom(a:Atom) extends Formula
case class BoxAtom(a:Atom) extends Formula
abstract case class AtAtom(t: Int, a: Atom) extends ExtendedAtom
abstract case class WAtAtom[X <: WindowParameters](w: WindowOperator[X], aa: AtAtom) extends ExtendedAtom
abstract case class WDiamAtom[X <: WindowParameters](w: WindowOperator[X], da: DiamAtom) extends ExtendedAtom
abstract case class WBoxAtom[X <: WindowParameters](w: WindowOperator[X], ba: BoxAtom) extends ExtendedAtom

abstract class Atom extends ExtendedAtom {
  //TODO terms list
  //override def equals(that:Any): Boolean =
  //  that match {
  //    case that: Atom => that.canEqual(this) && this.s.sameElements(that.asInstanceOf[Atom].s)
  //    case _ => false
  //  }
  override def toString = {
    this.getClass.getSimpleName
  }
}

//case class Atom0() extends Atom

case class Not(fm: Formula) extends Formula
case class And(fm1: Formula, fm2: Formula) extends Formula
case class Or(fm1: Formula, fm2: Formula) extends Formula
case class Implies(fm1: Formula, fm2: Formula) extends Formula
case class Diam(fm: Formula) extends Formula
case class Box(fm: Formula) extends Formula
case class At(t: Int, fm: Formula) extends Formula
//case class W[X <: WindowParameters](wfn: WindowFunction[X], ch: StreamChoice, x: X, fm: Formula) extends Formula
case class W[X <: WindowParameters](w: WindowOperator[X], fm: Formula) extends Formula

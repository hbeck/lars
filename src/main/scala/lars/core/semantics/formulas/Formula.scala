package lars.core.semantics.formulas

import lars.core.semantics.formulas.WindowOperators.{ch2, StreamChoice}
import lars.core.windowfn.{WindowFunction, WindowParameters}

/**
 * Created by hb on 5/26/15.
 */
abstract class Formula {

  def and(fm: Formula): Formula = And(this,fm)
  def or(fm: Formula): Formula = Or(this, fm)
  def implies(fm: Formula): Formula = Implies(this, fm)

  def par(fm: Formula) =
    if (fm.isInstanceOf[Binary]) "(" + fm.toString + ")"
    else fm.toString

  def atoms(): Set[Atom]
}

abstract class Unary(fm: Formula) extends Formula {
  override def atoms = fm.atoms()
}

abstract class Binary(fm1: Formula, fm2: Formula) extends Formula {
  override def atoms = fm1.atoms() ++ fm2.atoms()
}

abstract class ExtendedAtom extends Formula

abstract class Atom extends ExtendedAtom {
  //TODO terms list
  //override def equals(that:Any): Boolean =
  //  that match {
  //    case that: Atom => that.canEqual(this) && this.s.sameElements(that.asInstanceOf[Atom].s)
  //    case _ => false
  //  }
  override def toString = {
    val s = this.getClass.getSimpleName
    val idx = s.indexOf("$")
    if (idx == -1) s
    else s.substring(0,idx)
  }
  override def atoms() = Set(this)
}

//case class Atom0() extends Atom

case class Not(fm: Formula) extends Unary(fm) {
  override def toString = {
      "¬"+par(fm)
  }
}

case class And(fm1: Formula, fm2: Formula) extends Binary(fm1,fm2) {
  override def toString = par(fm1) + " ∧ " + par(fm2)
}

case class Or(fm1: Formula, fm2: Formula) extends Binary(fm1,fm2) {
  override def toString = par(fm1) + " ∨ " + par(fm2)
}

case class Implies(fm1: Formula, fm2: Formula) extends Binary(fm1,fm2) {
  override def toString = par(fm1) + " → " + par(fm2)
}

case class Diam(fm: Formula) extends Unary(fm) {
  override def toString = "◇"+par(fm)
}

case class Box(fm: Formula) extends Unary(fm) {
  override def toString = "☐"+par(fm)
}

case class At(t: Int, fm: Formula) extends Unary(fm) {
  override def toString = "@{"+t+"}"+par(fm)
}

case class W[X <: WindowParameters](wfn: WindowFunction[X], x: X, fm: Formula, ch: StreamChoice=ch2) extends Unary(fm) {
  override def toString = {
    val chStr = if (ch == ch2) "" else ","+ch
    "⊞_{"+wfn+chStr+"}^{"+x+"}"
  }
}

case class Wop(wop: WindowOperatorFixedParams, fm: Formula) extends Unary(fm) {
  override def toString = wop + par(fm)
}

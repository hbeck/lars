package lars.core.semantics.formulas

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
}

trait Binary

abstract class ExtendedAtom extends Formula

//TODO unify 'formula ontology' s.t. is doesnt need recasting later
case class DiamAtom(a:Atom) extends Formula {
  override def toString = "◇"+a
}
case class BoxAtom(a:Atom) extends Formula {
  override def toString = "☐"+a
}
case class AtAtom(t: Int, a: Atom) extends ExtendedAtom {
  override def toString = "@{"+t+"}"+a
}
case class WAtAtom(w: WindowOperator, aa: AtAtom) extends ExtendedAtom {
  override def toString = w + "" + aa
}
case class WDiamAtom(w: WindowOperator, da: DiamAtom) extends ExtendedAtom {
  override def toString = w + "" + da
}
case class WBoxAtom(w: WindowOperator, ba: BoxAtom) extends ExtendedAtom {
  override def toString = w + "" + ba
}

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
}

//case class Atom0() extends Atom

case class Not(fm: Formula) extends Formula {
  override def toString = {
      "¬"+par(fm)
  }
}
case class And(fm1: Formula, fm2: Formula) extends Formula with Binary {
  override def toString = par(fm1) + " ∧ " + par(fm2)
}
case class Or(fm1: Formula, fm2: Formula) extends Formula with Binary {
  override def toString = par(fm1) + " ∨ " + par(fm2)
}
case class Implies(fm1: Formula, fm2: Formula) extends Formula with Binary {
  override def toString = par(fm1) + " → " + par(fm2)
}
case class Diam(fm: Formula) extends Formula {
  override def toString = "◇"+par(fm)
}
case class Box(fm: Formula) extends Formula {
  override def toString = "☐"+par(fm)
}

case class At(t: Int, fm: Formula) extends Formula {
  override def toString = "@{"+t+"}"+par(fm)
}
//case class W[X <: WindowParameters](wfn: WindowFunction[X], ch: StreamChoice, x: X, fm: Formula) extends Formula
case class W(wop: WindowOperator, fm: Formula) extends Formula {
  override def toString = wop + par(fm)
}

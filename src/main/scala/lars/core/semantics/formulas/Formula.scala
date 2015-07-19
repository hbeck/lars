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

abstract class Unary(val fm: Formula) extends Formula {
  override def atoms = fm.atoms()
}

abstract class Binary(val fm1: Formula, val fm2: Formula) extends Formula {
  override def atoms = fm1.atoms() ++ fm2.atoms()
}

trait ExtendedAtom extends Formula {
  def atom:Atom
  def nested:Set[ExtendedAtom] = Set[ExtendedAtom](this,atom)
  override def atoms():Set[Atom] = Set[Atom](atom)
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
  override def atom = this
  override def nested = Set(this)
}

object Atom {}

object Verum extends Atom
object Falsum extends Atom

//case class Atom0() extends Atom

case class Not(override val fm: Formula) extends Unary(fm) {
  override def toString = {
      "¬"+par(fm)
  }
}

case class And(override val fm1: Formula, override val fm2: Formula) extends Binary(fm1,fm2) {
  override def toString = par(fm1) + " ∧ " + par(fm2)
}

object And {
  def apply[T <: Formula](formulas:Set[T]): Formula = {
    formulas.size match {
      case 0 => Verum
      case 1 => formulas.head
      case _ => { //does not work: formulas.reduce((x,y) => And(x,y))
        val seq: Seq[T] = formulas.toSeq
        var fm = And(seq(0),seq(1))
        for (i <- 2 to (seq.size-1)) {
          fm = And(fm,seq(i))
        }
        fm
      }
    }
  }
}

case class Or(override val fm1: Formula, override val fm2: Formula) extends Binary(fm1,fm2) {
  override def toString = par(fm1) + " ∨ " + par(fm2)
}

case class Implies(override val fm1: Formula, override val fm2: Formula) extends Binary(fm1,fm2) {
  override def toString = par(fm1) + " → " + par(fm2)
}

trait Temporal extends Unary

case class Diam(override val fm: Formula) extends Unary(fm) with Temporal {
  override def toString = "◇"+par(fm)
}

case class Box(override val fm: Formula) extends Unary(fm) with Temporal {
  override def toString = "☐"+par(fm)
}

case class At(t: Int, override val fm: Formula) extends Unary(fm) with Temporal {
  override def toString = "@{"+t+"}"+par(fm)
}

case class W[X <: WindowParameters](wfn: WindowFunction[X], x: X, override val fm: Formula, ch: StreamChoice=ch2) extends Unary(fm) {
  override def toString = {
    val chStr = if (ch == ch2) "" else ","+ch
    "⊞_{"+wfn+chStr+"}^{"+x+"}"
  }
}

case class WindowFormula(wop: WindowOperatorFixedParams, override val fm: Formula) extends Unary(fm) {
  override def toString = wop + par(fm)
}

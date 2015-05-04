package lars.core

import lars.core.WindowFunctions.{WindowParameters, WindowFunction}
import lars.core.WindowOperators.StreamChoice

/**
 * Created by hb on 1/2/15.
 */

object Formulas {

  abstract class Formula {
    def and(fm: Formula): Formula = And(this,fm)
    def or(fm: Formula): Formula = Or(this, fm)
    def implies(fm: Formula): Formula = Impl(this, fm)
    def not(): Formula = Not(this)
  }

  case class Atom(s: String) extends Formula
  case class Not(fm: Formula) extends Formula
  case class And(fm1: Formula, fm2: Formula) extends Formula
  case class Or(fm1: Formula, fm2: Formula) extends Formula
  case class Impl(fm1: Formula, fm2: Formula) extends Formula
  case class Diamond(fm: Formula) extends Formula
  case class Box(fm: Formula) extends Formula
  case class At(t: Int, fm: Formula) extends Formula
  case class Window[T <: WindowParameters, X <: WindowParameters](wfn: WindowFunction[T], ch: StreamChoice, x: X, fm: Formula) extends Formula



  def not(fm: Formula) = Not(fm)
  def box(fm: Formula) = Box(fm)
  def diamond(fm: Formula) = Diamond(fm)
  def at = (t:Int) => ((fm: Formula) => At(t,fm))
  def win[T <: WindowParameters, X <: WindowParameters] = (w:WindowFunction[T], ch: StreamChoice, x: X) => ((fm: Formula) => Window(w,ch,x,fm))


}


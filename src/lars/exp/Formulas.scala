package lars.exp

import lars.exp.WindowFunctions.WindowFunction
import lars.exp.Streams.Stream

/**
 * Created by hb on 1/2/15.
 */

object Formulas {

  abstract class Formula {
    //binary ops
    def and(fm: Formula): Formula = And(this,fm)
    def or(fm: Formula): Formula = Or(this, fm)
    def implies(fm: Formula): Formula = Impl(this, fm)
    def not(): Formula = Not(this)
  }

  //unary ops
  def not(fm: Formula) = Not(fm)
  def b(fm: Formula) = B(fm)
  def d(fm: Formula) = D(fm)
  def at = (t:Int) => ((fm: Formula) => At(t,fm))
  //val w_5 = win(tau,ch2,SeqArgs(Seq(0,5,1)))
  //case Win(w,ch,x,fm) => {
  //val S1 = w(ch(S0,S),t,x)
  //M/S1/t ||- a }
  def win = (w:WindowFunction, ch: Choice, x: Seq[Int]) => ((fm: Formula) => Win(w,ch,x,fm))

  case class Atom(s: String) extends Formula
  case class Not(fm: Formula) extends Formula
  case class And(fm1: Formula, fm2: Formula) extends Formula
  case class Or(fm1: Formula, fm2: Formula) extends Formula
  case class Impl(fm1: Formula, fm2: Formula) extends Formula
  case class D(fm: Formula) extends Formula
  case class B(fm: Formula) extends Formula
  case class At(t: Int, fm: Formula) extends Formula
  case class Win(wfn: WindowFunction, ch: Choice, x: Seq[Int], fm: Formula) extends Formula

  abstract class Choice() extends Function2[Stream,Stream,Stream]

  val ch1 = new Choice() {
    override def apply(s1: Stream, s2: Stream) : Stream = s1
  }

  val ch2 = new Choice() {
    override def apply(s1: Stream, s2: Stream) : Stream = s2
  }

}

